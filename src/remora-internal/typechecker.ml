(******************************************************************************)
(* Copyright (c) 2015, NVIDIA CORPORATION. All rights reserved.               *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided that the following conditions         *)
(* are met:                                                                   *)
(*  * Redistributions of source code must retain the above copyright          *)
(*    notice, this list of conditions and the following disclaimer.           *)
(*  * Redistributions in binary form must reproduce the above copyright       *)
(*    notice, this list of conditions and the following disclaimer in the     *)
(*    documentation and/or other materials provided with the distribution.    *)
(*  * Neither the name of NVIDIA CORPORATION nor the names of its             *)
(*    contributors may be used to endorse or promote products derived         *)
(*    from this software without specific prior written permission.           *)
(*                                                                            *)
(* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY       *)
(* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE          *)
(* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR         *)
(* PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR          *)
(* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,      *)
(* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,        *)
(* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR         *)
(* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY        *)
(* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT               *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE      *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.       *)
(******************************************************************************)

open Core.Std
open Basic_ast
open Core.Option
open Core.Option.Monad_infix
open Substitution


type 'a env = (var, 'a) List.Assoc.t with sexp

(* proper types only *)
type kind = unit with sexp

let env_update news olds =
  let dropped = List.fold ~init:olds ~f:(List.Assoc.remove ~equal:(=))
    (List.map ~f:fst news)
  in List.append news dropped
;;

(* Identify the sort of a type index *)
let rec srt_of_idx (idxs: srt env) (i: idx) : srt option =
  match i with
  | INat _ -> Some SNat
  | IShape dims ->
    let d_srts = List.map ~f:(srt_of_idx idxs) dims
    in if (List.for_all d_srts ~f:((=) (Some SNat)))
      then Some SShape else None
(* TODO: Maybe this form should just allow arbitrarily many operands? *)
  | ISum (left, right) ->
    let d_srts = List.map ~f:(srt_of_idx idxs) [left; right]
    in if (List.for_all d_srts ~f:((=) (Some SNat)))
      then Some SNat else None
  | IVar name -> List.Assoc.find idxs name
;;


let rec kind_of_typ (idxs: srt env)
                    (types: kind env)
                    (t: typ) : kind option =
  match t with
  | TFloat -> Some ()
  | TInt -> Some ()
  | TBool -> Some ()
  | TDProd (new_idxs, body)
    -> kind_of_typ (env_update new_idxs idxs) types body
  | TDSum (new_idxs, body)
    -> kind_of_typ (env_update new_idxs idxs) types body
  | TFun (ins, out)
(* This phrasing seems a little ugly *)
    -> if (List.for_all (List.map ~f:(kind_of_typ idxs types) ins) ~f:is_some)
      then kind_of_typ idxs types out
      else None
  | TArray (shape, elts) ->
    srt_of_idx idxs shape >>= fun i_srt ->
    if (i_srt = SShape)
    then (kind_of_typ idxs types elts >>= fun e_kind ->
          Some e_kind)
    else None
  | TAll (vars, body)
    -> kind_of_typ idxs
                   (env_update (List.map ~f:(fun x -> (x,())) vars) types)
                   body
  | TVar name -> List.Assoc.find types name
;;

(* Return the unique type in a list (or None if there is no such type)
   TODO: take type equality into account *)
let rec uniq_typ (typs: typ list) : (typ option) =
  match typs with
  | [] -> None
  | [t] -> Some t
  | t :: ts ->
    uniq_typ ts >>= fun (rest: typ) ->
    if (t = rest) then return t else None

(* Read the type annotation from an AST node *)
let typ_of_t_expr = function
  | AnnRExpr (t, _) -> t
let typ_of_t_elt = function
  | AnnRElt (t, _) -> t

(* Transform a list of options into an option of a list. If the original list
   contains any `None`s, collapse the whole list to a None.  *)
let rec option_list_xform (options: 'a option list) : 'a list option =
  match options with
  | [] -> Some []
  | None :: _ -> None
  | (Some a) :: more ->
    option_list_xform more >>= fun more_ ->
    a :: more_ |> return

(* Construct a shape from a list of natural numbers *)
let shape_of_nat_list nats = IShape (List.map ~f:(fun x -> INat x) nats)

(* Rewrite a shape in its fully-nested form *)
let rec expand_shape (s: idx) : idx list option =
  match s with
  | IVar _ as v -> Some [v]
  | IShape [] -> Some []
  | IShape (dim :: dims) ->
    (expand_shape (IShape dims)) >>= fun rest ->
    IShape [dim] :: rest |> return
  | INat _ | ISum _ -> None

(* Extract the fully nested shape of a type (non-arrays have scalar shape) *)
let rec shape_of_typ (t: typ) : idx list option =
  match t with
  | TArray (s, t) ->
    expand_shape s >>= fun s_ ->
    shape_of_typ t >>= fun t_ ->
    List.append s_ t_ |> return
  | _ -> Some []
let rec elt_of_typ (t: typ) : typ option =
  match t with
  | TArray (_, ((TArray (_, _)) as subarray)) -> elt_of_typ subarray
  | TArray (_, t) -> Some t
  | _ -> None

(* Canonicalize a type by rewriting TArrays in fully nested form. May reutrn
   None if given an array type with an invalid shape. *)
let rec canonicalize_typ = function
  | TVar _ | TInt | TFloat | TBool as v -> Some v
  | TDProd (bindings, body) ->
    canonicalize_typ body >>= fun new_body ->
    TDProd (bindings, new_body) |> return
  | TDSum (bindings, body) ->
    canonicalize_typ body >>= fun new_body ->
    TDSum (bindings, new_body) |> return
  | TAll (bindings, body) ->
    canonicalize_typ body >>= fun new_body ->
    TAll (bindings, new_body) |> return
  | TFun (args, ret) ->
    option_list_xform (List.map ~f:canonicalize_typ args) >>= fun args_ ->
    canonicalize_typ ret >>= fun ret_ ->
    TFun (args_, ret_) |> return
  | TArray (shape, (TArray (IShape [], elt_typ))) ->
    canonicalize_typ (TArray (shape, elt_typ))
  | TArray (IShape [], (TArray _ as sub_array)) -> canonicalize_typ sub_array
  | TArray (IShape [], elt_typ) ->
    canonicalize_typ elt_typ >>= fun elt_typ_ ->
    TArray (IShape [], elt_typ_) |> return
  | TArray (IVar v, (TArray _ as elt)) ->
    canonicalize_typ elt >>= fun sub_array ->
    TArray (IVar v, sub_array) |> return
  | TArray (IVar v, elt_typ) ->
    canonicalize_typ elt_typ >>= fun elt_typ_ ->
    TArray (IVar v, elt_typ_) |> return
  | TArray (IShape [dim], elt_typ) ->
    canonicalize_typ elt_typ >>= fun elt_canon ->
    TArray (IShape [dim], elt_canon) |> return
  | TArray (IShape (dim :: dims), elt_typ) ->
    canonicalize_typ (TArray (IShape dims, elt_typ)) >>= fun sub_array ->
    TArray (IShape [dim], sub_array) |> return
  | TArray _ -> None
(* Check whether two types are equivalent. *)
let typ_equal (t1: typ) (t2: typ) : bool =
  let rec typ_equal_ (t1: typ) (t2: typ) : bool =
    match (t1, t2) with
    | (TInt, TInt) | (TFloat, TFloat) | (TBool, TBool) -> true
    | (TDProd (bind1, body1), TDProd (bind2, body2))
    | (TDSum (bind1, body1), TDSum (bind2, body2)) ->
      if (List.length bind1) <> (List.length bind2) then false
      else
        let new_vars = List.map ~f:(fun _ -> gensym "__fresh_ivar=") bind1 in
        let new_body1 =
          idx_into_typ (List.zip_exn
                          (List.map ~f:fst bind1)
                          (List.map ~f:(fun x -> (IVar x)) new_vars)) body1
        and new_body2 =
          idx_into_typ (List.zip_exn
                          (List.map ~f:fst bind2)
                          (List.map ~f:(fun x -> (IVar x)) new_vars)) body2
        in typ_equal_ new_body1 new_body2
    | (TAll (bind1, body1), TAll (bind2, body2)) ->
      if (List.length bind1) <> (List.length bind2) then false
      else
        let new_vars = List.map ~f:(fun _ -> gensym "__fresh_tvar=") bind1 in
        let new_body1 =
          typ_into_typ
            (List.zip_exn bind1 (List.map ~f:(fun x -> (TVar x)) new_vars))
            body1
        and new_body2 =
          typ_into_typ
            (List.zip_exn bind2 (List.map ~f:(fun x -> (TVar x)) new_vars))
            body2
        in typ_equal_ new_body1 new_body2
    | (TArray (s1, elts1), TArray (s2, elts2)) ->
      (* TODO: check idx equality *)
      (s1 = s2) && (typ_equal_ elts1 elts2)
    (* | (TArray _, TArray _) -> *)
    (*   Option.value ~default:false (canonicalize_typ t1 >>= fun t1_ -> *)
    (*                                canonicalize_typ t2 >>= fun t2_ -> *)
    (*                                typ_equal_ t1_ t2_ |> return) *)
    | (TFun (args1, ret1), TFun (args2, ret2)) ->
      (List.for_all2_exn ~f:typ_equal_ args1 args2) && (typ_equal_ ret1 ret2)
    | (TVar x, TVar y) -> x = y
    | _ -> false
  in
  match (canonicalize_typ t1, canonicalize_typ t2) with
  | (Some t1_, Some t2_) -> typ_equal_ t1_ t2_
  | _ -> false

(* Idenfity the frame shape on a single argument based on its type *)
let rec frame_contribution (cell_typ: typ) (arg_typ: typ) : idx list option =
  canonicalize_typ cell_typ >>= fun ct_canonical ->
  canonicalize_typ arg_typ >>= fun at_canonical ->
  (* Base case: the types are equal *)
  if typ_equal ct_canonical at_canonical
  then Some [IShape []]
  (* Inductive case: peel away a layer of the actual type's shape *)
  else match at_canonical with
  | TArray (shp, elt_type) ->
    canonicalize_typ (TArray (IShape [], elt_type)) >>= fun elt_array ->
    if (typ_equal ct_canonical elt_array)
    then Some [shp]
    else frame_contribution cell_typ elt_type >>= fun (elt_contrib) ->
    shp :: elt_contrib |> return
  | _ -> None

(* Some true if xs is a prefix of ys, Some false if ys is a prefix of xs,
   otherwise, None. *)
let rec prefix_of (xs: 'a list) (ys: 'a list) : bool option =
  match (xs, ys) with
  | ([], _) -> Some true
  | (_, []) -> Some false
  | (x::xs_, y::ys_) ->
    if x = y then prefix_of xs_ ys_ else None
(* Choose which list is prefixed by the other *)
let prefixed (xs: 'a list) (ys: 'a list) : 'a list option =
  match prefix_of xs ys with
  | Some true -> Some ys
  | Some false -> Some xs
  | None -> None
(* Select the list element which is prefixed by all others, if there is one *)
let prefix_max (lsts: 'a list list) : 'a list option =
  match lsts with
  | [] -> None
  | [lst] -> Some lst
  | lst::lsts_ -> (List.fold_left
                     ~init:(Some lst)
                     ~f:(fun l r -> l >>= fun l_ -> prefixed l_ r)
                     lsts_)

(* Build an array type from a given nested shape *)
let build_array_type (shp: idx list) (elt: typ) : typ option =
  match shp with
  | [] ->TArray (IShape shp, elt) |> canonicalize_typ
  | _ -> (List.fold_right
            ~f:(fun i t -> TArray (i, t))
            ~init:elt shp) |> canonicalize_typ

(* Determine whether a type is a non-array (valid for use as an argument for
   a type abstraction).
   TODO: check whether this has to change to accommodate erasure *)
let non_array_type (t: typ) : bool =
  match t with
  | TArray _ -> false
  | _ -> true

(* Put a type annotation an an AST node *)
let rec annot_elt_type
    (idxs: srt env)
    (typs: kind env)
    (vars: typ env)
    (elt: 'a ann_elt) : (typ option) ann_elt =
  let (new_type, new_node) =
    match elt with AnnRElt (_, e) ->
      match e with
      | Int _ as e_ -> (Some TInt, e_)
      | Float _ as e_ -> (Some TFloat, e_)
      | Bool _ as e_ -> (Some TBool, e_)
      | Lam (bindings, body)
        -> (match (annot_expr_type idxs typs (env_update bindings vars)
                     body) with
        | AnnRExpr (Some t, _) as well_typed
          -> if (List.for_all
                   ~f:(fun (bind_type: typ) ->
                     Some () = kind_of_typ idxs typs bind_type)
                   (List.map ~f:snd bindings))
            then (Some (TFun (List.map ~f:snd bindings, t)),
                  Lam (bindings, well_typed))
            else (None, Lam (bindings, well_typed))
        | AnnRExpr (None, _) as ill_typed
          -> (None, Lam (bindings, ill_typed)))
      | Expr e -> let AnnRExpr (t_opt, _) as subexpr =
                    (annot_expr_type idxs typs vars e) in
                  (t_opt, Expr subexpr)
  in AnnRElt (new_type, new_node)
and annot_expr_type
    (idxs: srt env)
    (typs: kind env)
    (vars: typ env)
    (expr: 'a ann_expr) : (typ option) ann_expr =
  let (new_type, new_node): (typ option * (typ option ann_expr,
                                           typ option ann_elt) expr_form) =
    match expr with AnnRExpr (_, e) ->
      match e with
      | App (fn, args) ->
        let fn_annot = annot_expr_type idxs typs vars fn
        and args_annot = List.map ~f:(annot_expr_type idxs typs vars) args in
        let result_type =
          (* Fail if the function's element type is not well-formed. *)
          (typ_of_t_expr fn_annot) >>= shape_of_typ
          >>= fun (fun_shape : idx list) ->
          (* Fail if the function's shape is not well-formed. *)
          (match (typ_of_t_expr fn_annot) >>= elt_of_typ with
          | Some (TFun (cell_typs, ret_typ))
            -> Some (cell_typs, ret_typ)
          (* Fail if we don't have a well-typed function. *)
          | _ -> None) >>= fun (arg_expected_types, ret_cell_type) ->
          (* Fail if we don't have well-typed arguments. *)
          option_list_xform
            (List.map ~f:typ_of_t_expr args_annot) >>= fun arg_actual_types ->
          (* Identify each argument's contribution to the frame shape. *)
          option_list_xform
            (List.map2_exn ~f:frame_contribution
               arg_expected_types arg_actual_types) >>= fun arg_frame_shapes ->
          (* Find the largest under prefix ordering (the principal frame), if
             there is one. If not, fail. *)
          prefix_max (fun_shape :: arg_frame_shapes)
          >>= fun principal_frame_shape ->
          (* Assemble a type which wraps the application's principal frame
             around the function's result cell type. *)
          (build_array_type principal_frame_shape ret_cell_type)
        in (result_type, App (fn_annot, args_annot))
      | TApp (fn, typ_args) ->
        let fn_annot = annot_expr_type idxs typs vars fn in
        let result_type =
          match typ_of_t_expr fn_annot with
          | Some (TAll (tvars, tbody)) ->
            (* Fail if the type argument list is a different length than the
               type specifies. *)
            List.zip tvars typ_args >>= fun typ_subst ->
            Option.some_if
              (* all type args well-kinded *)
              (List.for_all
                 ~f:(fun t -> kind_of_typ idxs typs t = Some ()) typ_args
                (* all type args non-arrays *)
               && List.for_all ~f:non_array_type typ_args)
              (typ_into_typ typ_subst tbody)
          | _ -> None
        in (result_type, TApp (fn_annot, typ_args))
      | TLam (bindings, body) ->
        let env_extend = List.map ~f:(fun t -> (t, ())) bindings in
        let body_annot =
          annot_expr_type idxs (env_update env_extend typs) vars body in
        let tlam_type =
          typ_of_t_expr body_annot >>= fun body_type ->
          TAll (bindings, body_type) |> return
        in (tlam_type, TLam (bindings, body_annot))
      | IApp (fn, idx_args) ->
        let fn_annot = annot_expr_type idxs typs vars fn in
        let result_type =
          match typ_of_t_expr fn_annot with
          | Some (TDProd (ivar_bindings, tbody)) ->
            let ivars = List.map ~f:fst ivar_bindings
            and sorts = List.map ~f:snd ivar_bindings in
            List.zip ivars idx_args >>= fun idx_subst ->
            Option.some_if
              (* args all have correct sorts *)
              (List.for_all2_exn
                 ~f:(fun arg arg_sort -> srt_of_idx idxs arg = Some arg_sort)
                 idx_args sorts)
              (idx_into_typ idx_subst tbody)
          | _ -> None
        in (result_type, IApp (fn_annot, idx_args))
      | ILam (bindings, body) ->
        let body_annot =
          annot_expr_type (env_update bindings idxs) typs vars body in
        let ilam_type =
          typ_of_t_expr body_annot >>= fun body_type ->
          TDProd (bindings, body_type) |> return
        in (ilam_type, ILam (bindings, body_annot))
      | Arr (dims, data) ->
        let arr_size: int = List.fold_left ~f:( * ) ~init:1 dims
        and elts_annot = List.map ~f:(annot_elt_type idxs typs vars) data in
        let array_type =
          (* Fail if any dimension is negative or if they don't match the number
             of array elements we're given. *)
          (Option.some_if ((arr_size = List.length data)
                           && (List.for_all ~f:((<=) 0) dims))
             (shape_of_nat_list dims)) >>= fun array_shape ->
          (* Fail if any element is ill-typed. *)
          option_list_xform
            (List.map ~f:typ_of_t_elt
               elts_annot) >>= fun (elt_types: typ list) ->
          (* Fail if the elements don't all have the same type. *)
          uniq_typ elt_types >>=  fun (uniq_elt_type: typ) ->
          (* Use the shape idx constructed from the given dimensions and the
             derived unique element type to construct the array's type. *)
          return (TArray (array_shape, uniq_elt_type)) in
        (array_type, Arr (dims, elts_annot))
      | Var name as v_ -> (List.Assoc.find vars name, v_)
      | Pack (new_idxs, AnnRExpr (a, body), TDSum (ivars, t)) ->
        let AnnRExpr (body_typ, _) as body_annot =
          (annot_expr_type idxs typs vars (AnnRExpr (a, body)))
        in  (* Does every new_idx have the specified sort? *)
        if (List.map ~f:(srt_of_idx idxs) new_idxs)
          = (List.map ~f:(Fn.compose Option.some snd) ivars)
            (* Is the dependent sum's body well-typed? *)
            && (Option.is_some body_typ)
            (* Does substituting those indices into the declared type give the
               body's type? If the previous check passed, the lists we're
               zipping must have the same length. *)
            && (Option.value_exn body_typ)
            = (idx_into_typ (List.zip_exn (List.map ~f:fst ivars) new_idxs) t)
        then (Some (TDSum (ivars, t)),
              Pack (new_idxs, body_annot, TDSum (ivars, t)))
        else (None, Pack (new_idxs, body_annot, TDSum (ivars, t)))
      | Unpack (ivars, v, dsum, body) ->
        let (AnnRExpr (dsum_type, _)) as dsum_annot =
          annot_expr_type idxs typs vars dsum in
        let (witnesses, contents_binding) =
          (match dsum_type with
          | Some (TDSum (w, t)) -> (w, [v, t])
          | _ -> ([], [])) in
        let body_annot =
          annot_expr_type
            (env_update witnesses idxs)
            typs
            (env_update contents_binding vars)
            body in
        let result_type =
          typ_of_t_expr body_annot >>= fun body_type ->
          Option.some_if
            (kind_of_typ idxs typs body_type = Some ())
            body_type in
        (result_type, Unpack (ivars, v, dsum_annot, body_annot))
  in AnnRExpr (new_type, new_node)
;;

(* Convert a "maybe well-typed" AST into either Some typ ann_<tree> or None. *)
