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

(* representation of variables, may later want to add more info *)
type var = string with sexp

let gensym_counter = ref 0
let gensym_reset () = gensym_counter := 0;;
let gensym (v: string) : var =
  let id_number = !gensym_counter in
  let new_name = String.concat [v; string_of_int id_number]
  and _ = gensym_counter := 1 + id_number
  in new_name;;

(* Remora index sorts *)
type srt = SNat | SShape with sexp

(* Remora indices *)
type idx =
| INat of int
| IShape of idx list
| ISum of idx * idx
| IVar of var * srt option
with sexp

(* Shorthand for constructing index var nodes *)
let ivar n = IVar (n, None)
let nvar n = IVar (n, Some SNat)
let svar n = IVar (n, Some SShape)

(* Remora types *)
type typ =
| TFloat
| TInt
| TBool
| TDProd of (var * srt) list * typ
| TDSum of (var * srt) list * typ
| TFun of typ list * typ
| TArray of idx * typ
| TProd of typ list
| TAll of var list * typ
| TVar of var
with sexp

(* This is a two-way mutually recursive version of a trick described at
   http://lambda-the-ultimate.org/node/4170#comment-63836

   First, the syntax tree node type is parameterized over what a subnode
   (i.e., subexpression) looks like. Then, we create an explicitly recursive
   type for the syntax tree, where a subexpression is a tree node and an
   annotation (again, polymorphic in the annotation's type). This allows the
   structure to be built with `option rtype' annotations at first, and then
   annotated later with (non-option) `rtype', optional frame shape, etc. *)
(* General shape of a Remora expression *)
type ('self_t, 'elt_t) expr_form =
| App of 'self_t * 'self_t list
| TApp of 'self_t * typ list
| TLam of var list * 'self_t
| IApp of 'self_t * idx list
| ILam of (var * srt) list * 'self_t
| Let of var * 'self_t * 'self_t
| Arr of int list * 'elt_t list
| Tuple of 'self_t list
| Field of int * 'self_t
| LetTup of var list * 'self_t * 'self_t
| Var of var
(* Unfortunately, this typ becomes redundant once the AST is type-annotated. *)
| Pack of idx list * 'self_t * typ
| Unpack of var list * var * 'self_t * 'self_t
(* General shape of a Remora array element *)
and ('self_t, 'expr_t) elt_form =
| Float of float
| Int of int
| Bool of bool
| Lam of (var * typ) list * 'expr_t
| Expr of 'expr_t
with sexp

let map_expr_form
    ~(f_expr: 'old_self_t -> 'new_self_t)
    ~(f_elt: 'old_elt_t -> 'new_elt_t)
    (e: ('old_self_t, 'old_elt_t) expr_form)
    : ('new_self_t, 'new_elt_t) expr_form =
  match e with
  | App (fn, args) -> App (f_expr fn,
                           List.map ~f:f_expr args)
  | TApp (fn, t_args) -> TApp (f_expr fn, t_args)
  | TLam (t_vars, body) -> TLam (t_vars, f_expr body)
  | IApp (fn, i_args) -> IApp (f_expr fn, i_args)
  | ILam (i_vars, body) -> ILam (i_vars, f_expr body)
  | Let (var, bound, body) -> Let (var, f_expr bound, f_expr body)
  | Arr (dims, elts) -> Arr (dims, List.map ~f:f_elt elts)
  | Tuple elts -> Tuple (List.map ~f:f_expr elts)
  | Field (num, tup) -> Field (num, f_expr tup)
  | LetTup (vars, tup, body) -> LetTup (vars, f_expr tup, f_expr body)
  | Var _ as v -> v
  | Pack (idxs, v, t) -> Pack (idxs, f_expr v, t)
  | Unpack (ivars, v, dsum, body) -> Unpack (ivars, v, f_expr dsum, f_expr body)

let map_elt_form
    ~(f_expr: 'old_expr_t -> 'new_expr_t)
    (* ~(f_elt: 'old_self_t -> 'new_self_t) *)
    (l: ('old_self_t, 'old_expr_t) elt_form)
    : ('new_self_t, 'new_expr_t) elt_form =
  match l with
  | Float _ as f -> f
  | Int _ as i -> i
  | Bool _ as b -> b
  | Lam (vars, body) -> Lam (vars, f_expr body)
  | Expr e -> Expr (f_expr e)

(* Remora terms with no extra annotation field *)
type rem_expr =
| RExpr of (rem_expr, rem_elt) expr_form
and  rem_elt  =
| RElt  of (rem_elt, rem_expr) elt_form
with sexp
type rem_defn = RDefn of var * typ * rem_expr with sexp
type rem_prog = RProg of rem_defn list * rem_expr with sexp

(* Annotated Remora expression (parameterized over annotation type) *)
type 'annot ann_expr =
| AnnRExpr of 'annot * (('annot ann_expr), ('annot ann_elt)) expr_form
and  'annot ann_elt =
| AnnRElt of 'annot * (('annot ann_elt), ('annot ann_expr)) elt_form
with sexp
type 'annot ann_defn = AnnRDefn of var * typ * 'annot ann_expr with sexp
type 'annot ann_prog =
| AnnRProg of 'annot * 'annot ann_defn list * 'annot ann_expr
with sexp


(* Fully type-annotated Remora terms *)
type t_expr = typ ann_expr with sexp
type t_elt = typ ann_elt with sexp
type t_defn = typ ann_defn with sexp
type t_prog = typ ann_prog with sexp
(* Partially type-annotated Remora terms *)
type pt_expr = typ option ann_expr with sexp
type pt_elt = typ option ann_elt with sexp
type pt_defn = typ option ann_defn with sexp
type pt_prog = typ option ann_prog with sexp



(* For example,
    AnnRExpr ((TArray (IShape [2], TInt)),
              (Arr ([2], [AnnRElt (TInt, Int 3);
                          AnnRElt (TInt, Int 2)])));;
   is the type-annotated version of the 2-vector [2,3].

   With blank annotations (i.e. all annotations are ()),
    AnnRExpr ((), (Arr ([2], [AnnRElt ((), Int 3); AnnRElt ((), Int 2)])))

   With no annotations,
    RExpr (Arr ([2], [RElt (Int 3); RElt (Int 2)]))
*)

(* Set up a designated "blank" annotation at every AST node *)
let rec annot_expr_init ~(init: 'a) (expr: rem_expr) : 'a ann_expr =
  match expr with RExpr node ->
    AnnRExpr (init,
              map_expr_form
                ~f_expr:(annot_expr_init ~init:init)
                ~f_elt:(annot_elt_init ~init:init)
                node)
and annot_elt_init ~(init: 'a) (elt: rem_elt) : 'a ann_elt =
  match elt with RElt node ->
    AnnRElt (init,
             map_elt_form
               ~f_expr:(annot_expr_init ~init:init)
               node)
;;

(* Set up "blank" annotations in a definition *)
let annot_defn_init ~(init: 'a) (defn: rem_defn) : 'a ann_defn =
  let RDefn (n, t, v) = defn
  in AnnRDefn(n, t, annot_expr_init ~init:init v)

(* Set up "blank" annotations in a program *)
let annot_prog_init ~(init: 'a) (prog: rem_prog) : 'a ann_prog =
  let RProg (defns, expr) = prog in
  AnnRProg (init,
            List.map ~f:(annot_defn_init ~init:init) defns,
            annot_expr_init ~init:init expr)

(* Extract the non-annotated version of an AST node *)
let rec annot_expr_drop (expr: 'a ann_expr) : rem_expr =
  match expr with AnnRExpr (_, node) ->
    RExpr (map_expr_form ~f_expr:annot_expr_drop ~f_elt:annot_elt_drop node)
and annot_elt_drop (elt: 'a ann_elt) : rem_elt =
  match elt with AnnRElt (_, node) ->
    RElt (map_elt_form ~f_expr:annot_expr_drop node)
;;

(* Extract non-annotated version of a definition *)
let annot_defn_drop (defn: 'a ann_defn) : rem_defn =
  let AnnRDefn (n, t, v) = defn in
  RDefn (n, t, annot_expr_drop v)

(* Extract non-annotated version of a program *)
let annot_prog_drop (prog: 'a ann_prog) : rem_prog =
  let AnnRProg (_, defns, expr) = prog in
  RProg (List.map ~f:annot_defn_drop defns,
         annot_expr_drop expr)

let annot_of_expr ((AnnRExpr (annot, _)): 'a ann_expr) : 'a = annot
let annot_of_elt ((AnnRElt (annot, _)): 'a ann_elt) : 'a = annot
let annot_of_defn ((AnnRDefn (_, _, AnnRExpr (annot, _))): 'a ann_defn) : 'a
    = annot
let annot_of_prog ((AnnRProg (annot, _, _)): 'a ann_prog) : 'a = annot

(* Collect the passes which are essential to compilation. *)
module Passes : sig
  val prog : rem_prog -> unit ann_prog
  val defn : rem_defn -> unit ann_defn
  val expr : rem_expr -> unit ann_expr
  val elt : rem_elt -> unit ann_elt

  val prog_all : rem_prog -> unit ann_prog
  val defn_all : rem_defn -> unit ann_defn
  val expr_all : rem_expr -> unit ann_expr
  val elt_all : rem_elt -> unit ann_elt
end = struct
  let prog = annot_prog_init ~init:()
  let prog_all = prog

  let defn = annot_defn_init ~init:()
  let defn_all = defn

  let expr = annot_expr_init ~init:()
  let expr_all = expr

  let elt = annot_elt_init ~init:()
  let elt_all = elt
end
