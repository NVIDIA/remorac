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

(* Remora indices *)
type idx = INat of int | IShape of int list with sexp

(* Remora index sorts *)
type srt = SNat | SShape with sexp

(* Remora types *)
type typ =
| TFloat
| TInt
| TBool
| TDProd of (var * srt) list * typ
| TDSum of (var * srt) list * typ
| TFun of typ list * typ
| TArray of idx * typ
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
| Arr of int list * 'elt_t list
| Var of var
| Pack of idx list * 'self_t
| Unpack of var list * var * 'self_t * 'self_t
(* General shape of a Remora array element *)
and ('self_t, 'expr_t) elt_form =
| Float of float
| Int of int
| Bool of bool
| Lam of (var * typ) list * 'expr_t
| Expr of 'expr_t
with sexp

(* Annotated Remora expression (parameterized over annotation type) *)
type 'annot ann_expr = 
| AnnRExpr of 'annot * (('annot ann_expr), ('annot ann_elt)) expr_form
(* Annotated Remora array element *)
and  'annot ann_elt = 
| AnnRElt of 'annot * (('annot ann_elt), ('annot ann_expr)) elt_form
with sexp
(* Fully type-annotated Remora expression *)
type t_expr = typ ann_expr with sexp
(* Partially type-annotated Remora expression *)
type pt_expr = (typ option) ann_expr with sexp

(* Type-annotated Remora array element *)
type t_elt = typ ann_elt with sexp

(* Remora expression with no extra annotation field *)
type rem_expr =
| RExpr of (rem_expr, rem_elt) expr_form
and  rem_elt  =
| RElt  of (rem_elt, rem_expr) elt_form
with sexp

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
let rec annot_expr_init ~(init:'t) (expr: rem_expr) : 't ann_expr =
  match expr with RExpr node ->
    let new_node = match node with
       | App (fn, args)
	 -> App (annot_expr_init ~init:init fn,
		 List.map ~f:(annot_expr_init ~init:init) args)
       | TApp (fn, args)
	 -> TApp (annot_expr_init ~init:init fn, args)
       | TLam (args, body)
	 -> TLam (args, annot_expr_init ~init:init body)
       | IApp (fn, args)
	 -> IApp (annot_expr_init ~init:init fn, args)
       | ILam (args, body)
	 -> ILam (args, annot_expr_init ~init:init body)
       | Arr (shape, elts)
	 -> Arr (shape, (List.map ~f:(annot_elt_init ~init:init) elts))
       | Var name -> Var name
       | Pack (idxs, contents)
	 -> Pack (idxs, (annot_expr_init ~init:init contents))
       | Unpack (idxs, termvar, contents, body)
	 -> Unpack (idxs, termvar,
		    (annot_expr_init ~init:init contents),
		    (annot_expr_init ~init:init body))
    in AnnRExpr (init, new_node)
and annot_elt_init ~(init:'t) (elt: rem_elt) : 't ann_elt =
  match elt with RElt node ->
    let new_node = match node with
      | Lam (bindings, body) -> Lam (bindings, annot_expr_init ~init:init body)
      | Expr e -> Expr (annot_expr_init ~init:init e)
      | (Float _ | Int _ | Bool _) as other -> other
    in AnnRElt (init, new_node)
;;

(* Extract the non-annotated version of an AST node *)
let rec annot_expr_drop (expr: 't ann_expr) : rem_expr =
  match expr with AnnRExpr (_, node) ->
    let new_node = match node with
      | App (fn, args) -> App (annot_expr_drop fn,
			       List.map ~f:annot_expr_drop args)
      | TApp (fn, args) -> TApp (annot_expr_drop fn, args)
      | TLam (args, body) -> TLam (args, annot_expr_drop body)
      | IApp (fn, args) -> IApp (annot_expr_drop fn, args)
      | ILam (args, body) -> ILam (args, annot_expr_drop body)
      | Arr (shape, elts) -> Arr (shape, List.map ~f:annot_elt_drop elts)
      | Var name -> Var name
      | Pack (idxs, contents) -> Pack (idxs, annot_expr_drop contents)
      | Unpack (idxs, termvar, contents, body)
	-> Unpack (idxs, termvar,
		   annot_expr_drop contents,
		   annot_expr_drop body)
    in RExpr new_node
and annot_elt_drop (elt: 't ann_elt) : rem_elt =
  match elt with AnnRElt (_, node) ->
    let new_node = match node with
      | Lam (bindings, body) -> Lam (bindings, annot_expr_drop body)
      | Expr e -> Expr (annot_expr_drop e)
      | (Float _ | Int _ | Bool _) as other -> other
    in RElt new_node
;;
