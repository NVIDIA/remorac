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

val test_t_int : Basic_ast.typ
val test_t_float : Basic_ast.typ
val ta : Basic_ast.typ Basic_ast.ann_expr
val scalar_of_elt : Basic_ast.rem_elt -> Basic_ast.rem_expr
val scalar_of_expr : Basic_ast.rem_expr -> Basic_ast.rem_expr
val scalar_of_elt_form :
  (Basic_ast.rem_elt, Basic_ast.rem_expr) Basic_ast.elt_form ->
  Basic_ast.rem_expr
val scalar_of_expr_form :
  (Basic_ast.rem_expr, Basic_ast.rem_elt) Basic_ast.expr_form ->
  Basic_ast.rem_expr
val flat_arr_2_3 : Basic_ast.rem_expr
val flat_arr_0_4 : Basic_ast.rem_expr
val arr_2 : Basic_ast.rem_expr
val arr_wrong : Basic_ast.rem_expr
val nest_arr_2_3 : Basic_ast.rem_expr
val unary_lambda : Basic_ast.rem_elt
val binary_lambda : Basic_ast.rem_elt
val unary_app : Basic_ast.rem_expr
val binary_app : Basic_ast.rem_expr
val elt_of_expr_form :
  (Basic_ast.rem_expr, Basic_ast.rem_elt) Basic_ast.expr_form ->
  Basic_ast.rem_elt
val unary_to_nested_app : Basic_ast.rem_expr
val nested_to_unary_app : Basic_ast.rem_expr
val type_abst : Basic_ast.rem_expr
val type_abst_bad : Basic_ast.rem_expr
val type_app : Basic_ast.rem_expr
val index_abst : Basic_ast.rem_expr
val index_app : Basic_ast.rem_expr
val dep_sum_create : Basic_ast.rem_expr
val dep_sum_project : Basic_ast.rem_expr
val remora_compose : Basic_ast.rem_expr
val fork_compose : Basic_ast.rem_expr

module UnitTests : sig
  val suite_init_drop : OUnit2.test
end
