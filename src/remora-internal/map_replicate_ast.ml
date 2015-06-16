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

