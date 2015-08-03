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

module B = Basic_ast
open Frame_notes

type var = B.var with sexp

type idx = B.idx with sexp

type srt = B.srt with sexp

type typ =
| TInt
| TFloat
| TBool
| TVar
| TUnknown
| TFun of (typ list * typ)
| TDProd of ((var * srt) list * typ)
| TDSum of ((var * srt) list * typ)
| TArray of (idx * typ)
| TTuple of typ list
| TShape
with sexp
val of_typ : B.typ -> typ

val shape_of_typ : typ -> idx list option
val elt_of_typ : typ -> typ option
val typ_of_shape : idx list -> typ -> typ

type ('self_t, 'elt_t) expr_form =
| App of ('self_t * 'self_t list * typ)
| IApp of ('self_t * idx list)
| ILam of ((var * srt) list * 'self_t)
| Arr of (int list * 'elt_t list)
| Var of var
| Pack of (idx list * 'self_t)
| Unpack of (var list * var * 'self_t * 'self_t)
| Let of (var * 'self_t * 'self_t)
| Tuple of 'self_t list
| Field of int * 'self_t
| LetTup of (var list * 'self_t * 'self_t)
and ('self_t, 'expr_t) elt_form =
| Float of float
| Int of int
| Bool of bool
| Lam of (var list * 'expr_t)
| Expr of 'expr_t
with sexp

val map_expr_form :
  f_expr: ('old_self_t -> 'new_self_t)
  -> f_elt: ('old_elt_t -> 'new_elt_t)
  -> ('old_self_t, 'old_elt_t) expr_form
  -> ('new_self_t, 'new_elt_t) expr_form

val map_elt_form :
  f_expr: ('old_expr_t -> 'new_expr_t)
  -> ('old_self_t, 'old_expr_t) elt_form
  -> ('new_self_t, 'new_expr_t) elt_form

type erased_expr = EExpr of (erased_expr, erased_elt) expr_form
and erased_elt = EElt of (erased_elt, erased_expr) elt_form
with sexp
type erased_defn = EDefn of var * typ * erased_expr with sexp
type erased_prog = EProg of erased_defn list * erased_expr with sexp

type 'annot ann_expr =
| AnnEExpr of 'annot * ('annot ann_expr, 'annot ann_elt) expr_form
and 'annot ann_elt =
| AnnEElt of 'annot * ('annot ann_elt, 'annot ann_expr) elt_form
  with sexp
type 'annot ann_defn = AnnEDefn of var * typ * 'annot ann_expr with sexp
type 'annot ann_prog =
| AnnEProg of 'annot * 'annot ann_defn list * 'annot ann_expr
with sexp

val of_expr : B.rem_expr -> erased_expr
val of_elt : B.rem_elt -> erased_elt
val of_defn : B.rem_defn -> erased_defn
val of_prog : B.rem_prog -> erased_prog

val of_ann_expr :
  ?merge:('annot -> 'annot -> 'annot)
  -> 'annot B.ann_expr
  -> 'annot ann_expr
val of_ann_elt :
  ?merge:('annot -> 'annot -> 'annot)
  -> 'annot B.ann_elt
  -> 'annot ann_elt
val of_ann_defn :
  ?merge:('annot -> 'annot -> 'annot)
  -> 'annot B.ann_defn
  -> 'annot ann_defn
val of_ann_prog :
  ?merge:('annot -> 'annot -> 'annot)
  -> 'annot B.ann_prog
  -> 'annot ann_prog

val annot_expr_drop : 'annot ann_expr -> erased_expr
val annot_elt_drop : 'annot ann_elt -> erased_elt
val annot_defn_drop : 'annot ann_defn -> erased_defn
val annot_prog_drop : 'annot ann_prog -> erased_prog

val fix_expr_app_type : typ ann_expr -> typ ann_expr
val fix_elt_app_type : typ ann_elt -> typ ann_elt
val fix_defn_app_type : typ ann_defn -> typ ann_defn
val fix_prog_app_type : typ ann_prog -> typ ann_prog

val annot_of_expr : 'annot ann_expr -> 'annot
val annot_of_elt : 'annot ann_elt -> 'annot
val annot_of_defn : 'annot ann_defn -> 'annot
val annot_of_prog : 'annot ann_prog -> 'annot

val annot_expr_merge :
  ('a -> 'b -> 'c) -> 'a ann_expr -> 'b ann_expr -> 'c ann_expr option
val annot_elt_merge :
  ('a -> 'b -> 'c) -> 'a ann_elt -> 'b ann_elt -> 'c ann_elt option
val annot_defn_merge :
  ('a -> 'b -> 'c) -> 'a ann_defn -> 'b ann_defn -> 'c ann_defn option
val annot_prog_merge :
  ('a -> 'b -> 'c) -> 'a ann_prog -> 'b ann_prog -> 'c ann_prog option

val annot_expr_fmap : f:('a -> 'b) -> 'a ann_expr -> 'b ann_expr
val annot_elt_fmap : f:('a -> 'b) -> 'a ann_elt -> 'b ann_elt
val annot_defn_fmap : f:('a -> 'b) -> 'a ann_defn -> 'b ann_defn
val annot_prog_fmap : f:('a -> 'b) -> 'a ann_prog -> 'b ann_prog

module Passes : sig
  val prog :
    (B.typ * arg_frame * app_frame) B.ann_prog
    -> (typ * arg_frame * app_frame) ann_prog
  val defn :
    (B.typ * arg_frame * app_frame) B.ann_defn
    -> (typ * arg_frame * app_frame) ann_defn
  val expr :
    (B.typ * arg_frame * app_frame) B.ann_expr
    -> (typ * arg_frame * app_frame) ann_expr
  val elt :
    (B.typ * arg_frame * app_frame) B.ann_elt
    -> (typ * arg_frame * app_frame) ann_elt

  val prog_all : B.rem_prog -> (typ * arg_frame * app_frame) ann_prog option
  val defn_all : B.rem_defn -> (typ * arg_frame * app_frame) ann_defn option
  val expr_all : B.rem_expr -> (typ * arg_frame * app_frame) ann_expr option
  val elt_all : B.rem_elt -> (typ * arg_frame * app_frame) ann_elt option
end
