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

open Basic_ast

type app_frame =
| AppFrame of idx list
| NotApp
with sexp

val app_frame_of_option : idx list option -> app_frame
val option_of_app_frame : app_frame -> idx list option
val idxs_of_app_frame_exn : app_frame -> idx list

val annot_expr_app_frame : typ ann_expr -> app_frame ann_expr
val annot_elt_app_frame : typ ann_elt -> app_frame ann_elt
val annot_defn_app_frame : typ ann_defn -> app_frame ann_defn
val annot_prog_app_frame : typ ann_prog -> app_frame ann_prog


type arg_frame_rec = {frame: idx list; expansion: idx list} with sexp
type arg_frame =
| ArgFrame of arg_frame_rec
| NotArg
with sexp

val arg_frame_of_option : arg_frame_rec option -> arg_frame
val option_of_arg_frame : arg_frame -> arg_frame_rec option
val frame_of_arg_exn : arg_frame -> idx list
val expansion_of_arg_exn : arg_frame -> idx list

val annot_expr_arg_expansion :
  (app_frame * typ) ann_expr
  -> outer_expectation: typ option
  -> outer_frame: app_frame
  -> arg_frame ann_expr
val annot_elt_arg_expansion :
  (app_frame * typ) ann_elt
  -> arg_frame ann_elt
val annot_defn_arg_expansion :
  (app_frame * typ) ann_defn
  -> arg_frame ann_defn
val annot_prog_arg_expansion :
  (app_frame * typ) ann_prog
  -> arg_frame ann_prog
