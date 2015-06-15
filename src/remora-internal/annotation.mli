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

val annot_elt_merge :
  ('a -> 'b -> 'c)
  -> 'a ann_elt
  -> 'b ann_elt
  -> 'c ann_elt option
val annot_expr_merge :
  ('a -> 'b -> 'c)
  -> 'a ann_expr
  -> 'b ann_expr
  -> 'c ann_expr option
val annot_defn_merge :
  ('a -> 'b -> 'c)
  -> 'a ann_defn
  -> 'b ann_defn
  -> 'c ann_defn option
val annot_prog_merge :
  ('a -> 'b -> 'c)
  -> 'a ann_prog
  -> 'b ann_prog
  -> 'c ann_prog option

val annot_elt_fmap : f:('a -> 'b) -> 'a ann_elt -> 'b ann_elt
val annot_expr_fmap : f:('a -> 'b) -> 'a ann_expr -> 'b ann_expr
val annot_defn_fmap : f:('a -> 'b) -> 'a ann_defn -> 'b ann_defn
val annot_prog_fmap : f:('a -> 'b) -> 'a ann_prog -> 'b ann_prog
