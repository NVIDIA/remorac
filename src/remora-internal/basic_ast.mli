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

type var = bytes with sexp

type idx = 
| INat of int
| IShape of idx list
| ISum of idx * idx
| IVar of var
with sexp

type srt = SNat | SShape with sexp

type typ =
    TFloat
  | TInt
  | TBool
  | TDProd of (var * srt) list * typ
  | TDSum of (var * srt) list * typ
  | TFun of typ list * typ
  | TArray of idx * typ
  | TAll of var list * typ
  | TVar of var
with sexp

type ('self_t, 'elt_t) expr_form =
    App of 'self_t * 'self_t list
  | TApp of 'self_t * typ list
  | TLam of var list * 'self_t
  | IApp of 'self_t * idx list
  | ILam of (var * srt) list * 'self_t
  | Arr of int list * 'elt_t list
  | Var of var
  | Pack of idx list * 'self_t
  | Unpack of var list * var * 'self_t * 'self_t
and ('self_t, 'expr_t) elt_form =
    Float of float
  | Int of int
  | Bool of bool
  | Lam of (var * typ) list * 'expr_t
  | Expr of 'expr_t
with sexp

type 'annot ann_expr =
  AnnRExpr of 'annot * ('annot ann_expr, 'annot ann_elt) expr_form
and 'annot ann_elt =
  AnnRElt of 'annot * ('annot ann_elt, 'annot ann_expr) elt_form
with sexp

type t_expr = typ ann_expr with sexp

type pt_expr = typ option ann_expr with sexp

type t_elt = typ ann_elt with sexp

type rem_expr = RExpr of (rem_expr, rem_elt) expr_form
and rem_elt = RElt of (rem_elt, rem_expr) elt_form
with sexp

val annot_expr_init : init:'t -> rem_expr -> 't ann_expr
val annot_elt_init : init:'t -> rem_elt -> 't ann_elt
val annot_expr_drop : 't ann_expr -> rem_expr
val annot_elt_drop : 't ann_elt -> rem_elt
