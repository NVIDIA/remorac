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
      then Some SShape else None
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
    srt_of_idx idxs shape >>= fun _ ->
    kind_of_typ idxs types elts >>= fun e_kind ->
    Some e_kind
  | TAll (vars, body)
    -> kind_of_typ idxs
                   (env_update (List.map ~f:(fun x -> (x,())) vars) types)
                   body
  | TVar name -> List.Assoc.find types name
;;

(* Maybe better to operate on annotated AST only -- doesn't seem to be a clear,
   performant way to lift this into a form that will annotate the AST. Should
   not repeatedly check same subtree (use annotation to store subtree results).
*)
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
      (* TODO: include well-formedness check for types the vars are bound at *)
      | Lam (bindings, body)
        -> (match (annot_expr_type idxs typs (env_update bindings vars) body) with
        | AnnRExpr (Some t, _) as well_typed
          -> (Some (TFun (List.map ~f:snd bindings, t)),
              Lam (bindings, well_typed))
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
            && t = (idx_into_typ
                      (List.zip_exn (List.map ~f:fst ivars) new_idxs)
                      (Option.value_exn body_typ))
        then (Some (TDSum (ivars, t)),
              Pack (new_idxs, body_annot, TDSum (ivars, t)))
        else (None, Pack (new_idxs, body_annot, TDSum (ivars, t)))
  in AnnRExpr (new_type, new_node)
;;
