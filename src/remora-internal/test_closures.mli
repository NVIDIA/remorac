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

open Closures
open Core.Std
module MR = Map_replicate_ast;;
module B = Basic_ast;;
module U = OUnit2;;
open Frame_notes;;

val unary_lam : expr

val mr_wrap : (E.typ * arg_frame * app_frame) MR.ann_expr MR.expr_form
  -> (E.typ * arg_frame * app_frame) MR.ann_expr

val escaping_function : (E.typ * arg_frame * app_frame) MR.ann_expr

val converted : expr

val destr_dsum : (E.typ * arg_frame * app_frame) MR.ann_expr
val vec_scal_add : (E.typ * arg_frame * app_frame) MR.ann_expr

val subst : (var, expr) List.Assoc.t -> expr -> expr

val alpha_eqv : expr -> expr -> bool

val expr_unhoist : ('a ann_expr, 'a) Defn_writer.t -> expr
val hoist_unhoist : 'a ann_expr -> bool

val lambda_free : 'a ann_expr -> bool
val defn_lambda_free : 'a ann_defn -> bool
val writer_lambda_free : ('a ann_expr, 'a) Defn_writer.t -> bool

module Test_closure_conversion : sig
  val tests : U.test
end

module Test_lambda_hoisting : sig
  val tests : U.test
end

module UnitTests : sig
  val tests : U.test
end
