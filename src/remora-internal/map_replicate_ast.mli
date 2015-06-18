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
module E = Erased_ast

type var = B.var with sexp

type 'a app_t = {fn : 'a; args: 'a list;} with sexp
type 'a vec_t = {dims: int list; elts: 'a list;} with sexp
type 'a map_t = {frame: 'a; fn: 'a; args: 'a list; shp: 'a;} with sexp
type 'a rep_t = {arg: 'a; new_frame: 'a; old_frame: 'a;} with sexp
type 'a tup_t = 'a list with sexp
type 'a let_t = {vars: var list; bound: 'a; body: 'a;} with sexp
type 'a lam_t = {bindings: var list; body: 'a;} with sexp

type 'a expr_form =
    App of 'a app_t
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

type expr = Expr of expr expr_form with sexp
type defn = Defn of var * expr with sexp
type prog = Prog of defn tup_t * expr with sexp

type 'annot ann_expr = AExpr of 'annot * 'annot ann_expr expr_form with sexp
type 'annot ann_defn = ADefn of var * 'annot ann_expr with sexp
type 'annot ann_prog =
  AProg of 'annot * 'annot ann_defn tup_t * 'annot ann_expr with sexp

val op_name_plus : var
val op_name_append : var

val of_erased_idx :
  E.idx -> (Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr

val of_nested_shape :
  E.idx tup_t -> (Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr

val defunctionalized_map :
  fn:(Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr ->
  args:(Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr tup_t ->
  shp:(Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr ->
  frame:(Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr ->
  (Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr expr_form

val of_erased_expr :
  (Frame_notes.arg_frame * Frame_notes.app_frame) E.ann_expr ->
  (Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr

val of_erased_elt :
  (Frame_notes.arg_frame * Frame_notes.app_frame) E.ann_elt ->
  (Frame_notes.arg_frame * Frame_notes.app_frame) ann_expr

val annot_expr_drop : 'a ann_expr -> expr
