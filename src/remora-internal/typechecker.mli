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

type 'a env = (var, 'a) Core.Std.List.Assoc.t with sexp
type kind = unit with sexp

val env_update : 'a env -> 'a env -> 'a env

val srt_of_idx : srt env -> idx -> srt option

val kind_of_typ : srt env -> kind env -> typ -> kind option

val uniq_typ : typ list -> typ option

val expand_shape : idx -> idx list option

val shape_drop : idx list -> idx list -> idx list option

val shape_of_typ : typ -> idx list option
val elt_of_typ : typ -> typ option

val idx_equal : idx -> idx -> bool

val canonicalize_typ : typ -> typ option

val prefix_of : 'a list -> 'a list -> bool option

val typ_equal : typ -> typ -> bool

val frame_contribution  : typ -> typ -> idx list option

val typ_of_shape : typ -> idx list -> typ
val canonical_typ_of_shape : typ -> idx list -> typ option

val annot_elt_type : srt env -> kind env -> typ env -> 'a ann_elt
  -> typ option ann_elt
val annot_expr_type : srt env -> kind env -> typ env -> 'a ann_expr
  -> typ option ann_expr
val annot_defn_type : srt env -> kind env -> typ env -> 'a ann_defn
  -> typ option ann_defn
val annot_prog_type : srt env -> kind env -> typ env -> 'a ann_prog
  -> pt_prog

val well_typed_of_expr : typ option ann_expr -> typ ann_expr option
val well_typed_of_elt : typ option ann_elt -> typ ann_elt option
val well_typed_of_defn: typ option ann_defn -> typ ann_defn option
val well_typed_of_prog : typ option ann_prog -> typ ann_prog option

module Passes : sig
  val prog : 'a ann_prog -> typ ann_prog option
  val defn : 'a ann_defn -> typ ann_defn option
  val expr : 'a ann_expr -> typ ann_expr option
  val elt : 'a ann_elt -> typ ann_elt option

  val prog_all : rem_prog -> typ ann_prog option
  val defn_all : rem_defn -> typ ann_defn option
  val expr_all : rem_expr -> typ ann_expr option
  val elt_all : rem_elt -> typ ann_elt option
end
