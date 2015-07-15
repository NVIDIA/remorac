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
open Frame_notes
module B = Basic_ast;;
module E = Erased_ast;;
module T = Typechecker;;

(* TODO: Want a new form of "erased type" that can handle indices, which may
   be int vectors of unknown length. For now, using E.TArray (IVar "", TUnknown)
   for this. *)
let shape_t = E.TArray (B.IVar "", E.TBase)
let sum_t = E.TFun ([E.TBase; E.TBase], E.TBase)
let append_t = E.TFun ([E.TBase; E.TBase], E.TBase)

type var = Basic_ast.var with sexp

(* In this stage, we eliminate the term/index distinction. Some expression
   forms are getting fairly crowded with sub-expressions that have different
   roles. *)
(* Ordinary function application, no implicit lifting. *)
type 'a app_t = {fn: 'a; args: 'a list} with sexp
(* Vector construction notation. *)
type 'a vec_t = {dims: int list; elts: 'a list} with sexp
(* Break args into cells according to the given frame shape, and map the given
   function across corresponding cells in each arg. If the frame is empty (i.e.,
   no result cells will be produced), produce an array of designated shape. *)
type 'a map_t = {frame: 'a; fn: 'a; args: 'a list; shp: 'a} with sexp
(* Replicate an array's cells a given number of times. *)
type 'a rep_t = {arg: 'a; new_frame: 'a; old_frame: 'a} with sexp
(* Ordinary tuples. *)
type 'a tup_t = 'a list with sexp
(* Let-binding a tuple's contents. *)
type 'a let_t = {vars: var list; bound: 'a; body: 'a} with sexp
(* Ordinary (non-lifting) functions. *)
type 'a lam_t = {bindings: var list; body: 'a} with sexp

type 'a expr_form =
| App of 'a app_t
| Vec of 'a vec_t
| Map of 'a map_t
| Rep of 'a rep_t
| Tup of 'a tup_t
| Let of 'a let_t
| Lam of 'a lam_t
| Var of var
| Int of int
| Float of float
| Bool of bool
with sexp

let map_expr_form ~f = function
  | App {fn = fn; args = args} -> App {fn = f fn; args = List.map ~f:f args}
  | Vec {dims = dims; elts = elts}
    -> Vec {dims = dims;
            elts = List.map ~f:f elts}
  | Map {frame = frame; fn = fn; args = args; shp = shp}
    -> Map {frame = f frame;
            fn = f fn;
            args = List.map ~f:f args;
            shp = f shp}
  | Rep {arg = arg; new_frame = new_frame; old_frame = old_frame}
    -> Rep {arg = f arg; new_frame = f new_frame; old_frame = f old_frame}
  | Tup elts -> Tup (List.map ~f:f elts)
  | Let {vars = vars; bound = bound; body = body}
    -> Let {vars = vars; bound = f bound; body = f body}
  | Lam {bindings = bindings; body = body}
    -> Lam {bindings = bindings; body = f body}
  | Var _ | Int _ | Float _ | Bool _ as v -> v

type expr = Expr of expr expr_form with sexp
type defn = Defn of var * expr with sexp
type prog = Prog of defn list * expr with sexp

type 'annot ann_expr = AExpr of 'annot * ('annot ann_expr) expr_form with sexp
type 'annot ann_defn = ADefn of var * 'annot ann_expr with sexp
type 'annot ann_prog =
  AProg of 'annot * 'annot ann_defn list * 'annot ann_expr with sexp

(* Names for some primitive operations this IR relies on. *)
let op_name_plus : var = "+"
let op_name_append : var = "append"

(* Convert a type-erased AST into a Map/Replicate AST. The input AST is expected
   to have annotations for type, application, and argument frames. *)
let rec of_erased_idx (i: B.idx) : (E.typ * arg_frame * app_frame) ann_expr =
  match i with
  | B.INat n -> AExpr ((E.TBase, NotArg, NotApp), Int n)
  (* TODO: Make sure the programmer doesn't shadow this operator. *)
  | B.ISum (i1, i2) ->
    AExpr ((E.TBase, NotArg, NotApp),
           App {fn = AExpr ((sum_t, NotArg, NotApp),
                            Var op_name_plus);
                args = [of_erased_idx i1; of_erased_idx i2]})
  | B.IShape idxs ->
    AExpr ((shape_t, NotArg, NotApp),
           Vec {dims = [List.length idxs];
                elts = List.map ~f:of_erased_idx idxs})
  (* We no longer see the checking environment. Maybe indices should
     have been sort-annotated from the beginning? *)
  | B.IVar name -> AExpr ((E.TUnknown, NotArg, NotApp), Var ("__I_" ^ name))

let of_nested_shape (idxs: E.idx list)
    : (E.typ * arg_frame * app_frame) ann_expr =
  List.map ~f:of_erased_idx idxs
  |> List.fold_right
      ~init:(of_erased_idx (B.IShape []))
      ~f:(fun l r -> (AExpr ((shape_t, NotArg, NotApp),
                             App {fn = AExpr ((append_t, NotArg, NotApp),
                                              Var op_name_append);
                                  args = [l; r]})))



(* TODO: this pass type checks, but does it work? *)

(* Generate a "defunctionalized" map to handle an array-of-functions. Requires
   all arguments to be fully Replicated. *)
let defunctionalized_map
    ~(fn: (E.typ * arg_frame * app_frame) ann_expr)
    ~(args: (E.typ * arg_frame * app_frame) ann_expr list)
    ~(shp: (E.typ * arg_frame * app_frame) ann_expr)
    ~(frame: (E.typ * arg_frame * app_frame) ann_expr) =
  let __ t = (t, NotArg, NotApp)
  and fn_var = B.gensym "__FN_"
  and arg_vars = List.map ~f:(fun _ -> B.gensym "__ARG_") args
  and (AExpr ((fn_typ,_,_),_)) = fn in
  (* Need to extract the input and output cell types from fn_typ.
     TODO: if not array of functions, what do? *)
  let (in_cell_typs, out_cell_typ) = (match E.elt_of_typ fn_typ with
    | Some E.TFun ((_, _) as t) -> t
    | _ ->
      print_string
        "Warning: generated Lam with non-TFun type annotation";
      ([E.TUnknown], E.TUnknown)) in
  let fn_cell_typ = E.TFun (in_cell_typs, out_cell_typ) in
  let apply_lam =
    (* If we're defunctionalizing an array of (t1 ... -> t2), then
       apply's type is ((t1 ... -> t2) t1 ... -> t2)  *)
    AExpr (__ (E.TFun (fn_cell_typ :: in_cell_typs, out_cell_typ)),
           Lam {bindings = fn_var :: arg_vars;
                body = AExpr (__ out_cell_typ,
                              App {fn = AExpr (__ fn_cell_typ,
                                               Var fn_var);
                                   (* Each arg  *)
                                   args = (List.map2_exn
                                             ~f:(fun v t -> AExpr (__ t, Var v))
                                             arg_vars in_cell_typs)})}) in
  Map {fn = apply_lam;
       args = fn :: args;
       shp = shp;
       frame = frame}
let rec of_erased_expr
    (E.AnnEExpr ((typ, arg, app), e):
       (E.typ * arg_frame * app_frame) E.ann_expr)
    : (E.typ * arg_frame * app_frame) ann_expr =
  AExpr ((typ, arg, app),
         match e with
         | E.Var name -> Var name
         | E.ILam (bindings, body) ->
           Lam {bindings = List.map
               ~f:(fun (name, _) -> ("__I_" ^ name)) bindings;
                body = of_erased_expr body}
         | E.IApp (fn, args) -> App {fn = of_erased_expr fn;
                                     args = List.map ~f:of_erased_idx args}
         (* Note: the value has moved to the front of the tuple. *)
         | E.Pack (idxs, value)
           -> Tup (of_erased_expr value :: List.map ~f:of_erased_idx idxs)
         | E.Unpack (ivars, v, dsum, body) -> Let {vars = v :: ivars;
                                                   bound = of_erased_expr dsum;
                                                   body = of_erased_expr body}
         (* TODO: Some call to Option.value_exn in this branch was failing.
            Is it fixed? *)
         | E.App (fn, args, shp) ->
           let app_frame_shape = of_nested_shape (idxs_of_app_frame_exn app) in
           (* How to lift an argument into the application form's frame. *)
           let lift (E.AnnEExpr ((typ, argf, appf), _) as a)
               : (E.typ * arg_frame * app_frame) ann_expr =
             match argf with
             | NotArg -> assert false
             | ArgFrame {frame = fr; expansion = ex;} ->
             let compute_old_frame = of_nested_shape fr
             and target_frame = List.append fr ex in
             AExpr ((E.typ_of_shape (idxs_of_app_frame_exn app) typ,
                     ArgFrame {frame = target_frame;
                               expansion = []},
                     appf),
                    Rep {arg = of_erased_expr a;
                         new_frame = app_frame_shape;
                         old_frame = compute_old_frame})
           (* Identify the function array's frame. If it's scalar, everything's
              simple. If it's not, we need to replace it with a scalar. *)
           and fn_frame = frame_of_arg_exn (Tuple3.get2 (E.annot_of_expr fn)) in
           (* TODO: Relax this equality check to make sure we're not mapping
              on something like [IShape []; IShape []]. *)
           if fn_frame = []
           then
             Map {frame = app_frame_shape;
                  fn = of_erased_expr fn;
                  shp = of_nested_shape (Option.value
                                           ~default:[B.IShape []]
                                           (E.shape_of_typ shp));
                  args = List.map ~f:lift args}
           else
             defunctionalized_map
               ~frame:app_frame_shape
               ~fn:(of_erased_expr fn)
               ~shp:(of_nested_shape
                       (Option.value
                          ~default:[B.IShape []]
                          (E.shape_of_typ shp)))
               ~args:(List.map ~f:lift args)
         | E.Arr (dims, elts) ->
           Vec {dims = dims;
                elts = List.map ~f:of_erased_elt elts}
  )
and of_erased_elt
    (E.AnnEElt ((typ, arg, app), e): (E.typ * arg_frame * app_frame) E.ann_elt)
    : (E.typ * arg_frame * app_frame) ann_expr =
  match e with
  | E.Expr (exp) -> of_erased_expr exp
  | _ -> AExpr ((typ, arg, app),
                match e with
                (* Already handled this case, so silence the
                   exhaustiveness warning for it. *)
                | E.Expr _ -> assert false
                | E.Lam (bindings, body) -> Lam {bindings = bindings;
                                                 body = of_erased_expr body}
                | E.Int i -> Int i
                | E.Float f -> Float f
                | E.Bool b -> Bool b)
let of_erased_defn
    (E.AnnEDefn (n, _, v): (E.typ * arg_frame * app_frame) E.ann_defn)
    : (E.typ * arg_frame * app_frame) ann_defn =
  ADefn (n, of_erased_expr v)
let of_erased_prog
    (E.AnnEProg (annot, defns, expr):
       (E.typ * arg_frame * app_frame) E.ann_prog)
    : (E.typ * arg_frame * app_frame) ann_prog =
  AProg (annot, List.map ~f:(of_erased_defn) defns, of_erased_expr expr)

(* For debugging help, a pass to drop annotations. *)
let rec annot_expr_drop (AExpr (_, e)) =
  Expr (map_expr_form ~f:annot_expr_drop e)
let annot_defn_drop (ADefn (n, v)) =
  Defn (n, annot_expr_drop v)
let annot_prog_drop (AProg (_, defns, e)) =
  Prog (List.map ~f:annot_defn_drop defns, annot_expr_drop e)

(* Remove all occurrences of an element from a list *)
(* let rec remove x xs = *)
(*   match xs with *)
(*   | [] -> [] *)
(*   | y :: ys -> if (y = x) then remove x ys *)
(*     else y :: (remove x ys) *)
(* let remove_all xs ys = *)
(*   List.fold_right ~init:ys ~f:(fun l r -> remove l r) ys *)

(* Identify all variables which appear free in an expression. *)
let get_free_vars
    (bound: var list)
    (recur: var list -> 'a -> var list)
    (e: 'a expr_form) : var list =
  let dedup = List.dedup ~compare:String.compare in
  match e with
  | App {fn = f; args = a} ->
    (recur bound f) :: (List.map ~f:(recur bound) a) |>
        List.concat |> dedup
  | Vec {dims = _; elts = l} ->
    List.map ~f:(recur bound) l |> List.concat |> dedup
  | Map {frame = fr; fn = fn; args = a; shp = s} ->
    (recur bound fr) :: (recur bound fn) :: (recur bound s) ::
      List.map ~f:(recur bound) a |>
          List.concat |> dedup
  | Rep {arg = a; old_frame = o; new_frame = n} ->
    List.append (recur bound a)
      (List.append (recur bound o) (recur bound n)) |>
        dedup
  | Tup l -> List.map ~f:(recur bound) l |> List.concat |> dedup
  | Lam {bindings = v; body = b} ->
    let bound_ = List.dedup (List.append bound v) in
    recur bound_ b
  | Let {vars = v; bound = bn; body = bd} ->
    let bound_ = dedup (List.append bound v) in
    List.append (recur bound bn) (recur bound_ bd) |> dedup
  | Var n -> if List.mem bound n then [] else [n]
  | Int _ | Float _ | Bool _ -> []
let rec aexpr_free_vars
    (bound: var list)
    (AExpr (_, expr): 'a ann_expr) : var list =
  get_free_vars bound aexpr_free_vars expr
(* Get annotated AST nodes for the free vars in an expression *)
let rec get_annotated_free_vars
    (bound: var list)
    ((AExpr (_, e)) as ast: 'a ann_expr) : 'a ann_expr list =
  (* Suppress warning about not explicitly passing a comparison function
     for every |> use of List.dedup *)
  let dedup a = List.dedup a in
  match e with
  | App {fn = f; args = a} ->
    (get_annotated_free_vars bound f)
    :: List.map ~f:(get_annotated_free_vars bound) a |>
        List.concat |> dedup
  | Vec {dims = _; elts = l} ->
    List.map ~f:(get_annotated_free_vars bound) l |> List.concat |> dedup
  | Map {frame = fr; fn = fn; args = a; shp = s} ->
    (get_annotated_free_vars bound fr)
    :: (get_annotated_free_vars bound fn)
    :: (get_annotated_free_vars bound s)
    :: List.map ~f:(get_annotated_free_vars bound) a |>
        List.concat |> dedup
  | Rep {arg = a; old_frame = o; new_frame = n} ->
    List.append (get_annotated_free_vars bound a)
      (List.append (get_annotated_free_vars bound o)
         (get_annotated_free_vars bound n)) |> dedup
  | Tup l ->
    List.map ~f:(get_annotated_free_vars bound) l |> List.concat |> dedup
  | Lam {bindings = v; body = b} ->
    let bound_ = List.dedup (List.append bound v) in
    get_annotated_free_vars bound_ b
  | Let {vars = v; bound = bn; body = bd} ->
    let bound_ = List.dedup (List.append bound v) in
    List.append
      (get_annotated_free_vars bound bn)
      (get_annotated_free_vars bound_ bd) |> dedup
  | Var n -> if List.mem bound n then [] else [ast]
  | Int _ | Float _ | Bool _ -> []

module Passes : sig
  val prog :
    (E.typ * arg_frame * app_frame) E.ann_prog
    -> (E.typ * arg_frame * app_frame) ann_prog
  val defn :
    (E.typ * arg_frame * app_frame) E.ann_defn
    -> (E.typ * arg_frame * app_frame) ann_defn
  val expr :
    (E.typ * arg_frame * app_frame) E.ann_expr
    -> (E.typ * arg_frame * app_frame) ann_expr
  val elt :
    (E.typ * arg_frame * app_frame) E.ann_elt
    -> (E.typ * arg_frame * app_frame) ann_expr

  val prog_all : B.rem_prog -> (E.typ * arg_frame * app_frame) ann_prog option
  val defn_all : B.rem_defn -> (E.typ * arg_frame * app_frame) ann_defn option
  val expr_all : B.rem_expr -> (E.typ * arg_frame * app_frame) ann_expr option
  val elt_all : B.rem_elt -> (E.typ * arg_frame * app_frame) ann_expr option
end = struct
  open Option.Monad_infix
  (* There doesn't seem to be a way to carry the typ component along without
     just reworking the translation pass itself. *)
  let prog (remora: (E.typ * arg_frame * app_frame) E.ann_prog)
      : (E.typ * arg_frame * app_frame) ann_prog =
    remora |> of_erased_prog
  let prog_all (remora: B.rem_prog)
      : (E.typ * arg_frame * app_frame) ann_prog option =
    remora |> E.Passes.prog_all >>| prog

  let defn (remora: (E.typ * arg_frame * app_frame) E.ann_defn)
      : (E.typ * arg_frame * app_frame) ann_defn =
    remora |> of_erased_defn
  let defn_all (remora: B.rem_defn)
      : (E.typ * arg_frame * app_frame) ann_defn option =
    remora |> E.Passes.defn_all >>| defn

  let expr (remora: (E.typ * arg_frame * app_frame) E.ann_expr)
      : (E.typ * arg_frame * app_frame) ann_expr =
    remora |> of_erased_expr
  let expr_all (remora: B.rem_expr)
      : (E.typ * arg_frame * app_frame) ann_expr option =
    remora |> E.Passes.expr_all >>| expr

  let elt (remora: (E.typ * arg_frame * app_frame) E.ann_elt)
      : (E.typ * arg_frame * app_frame) ann_expr =
    remora |> of_erased_elt
  let elt_all (remora: B.rem_elt)
      : (E.typ * arg_frame * app_frame) ann_expr option =
    remora |> E.Passes.elt_all >>| elt
end
