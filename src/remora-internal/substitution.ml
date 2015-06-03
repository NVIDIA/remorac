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
open Core.Std

(* Finite mapping from variable to something -- same as an environment *)
type 'a subst = (var, 'a) List.Assoc.t with sexp

(* May later need...
   let rec exp_into_expr (sub: rem_expr subst) (x: rem_expr) : rem_expr
   let rec exp_into_elt (sub: rem_expr subst) (l: rem_elt) : rem_elt
   let rec elt_into_expr (sub: rem_elt subst) (x: rem_expr) : rem_expr
   let rec elt_into_elt (sub: rem_elt subst) (l: rem_elt) : rem_elt
   let rec typ_into_expr (typs: typ subst) (x: rem_expr) : rem_expr
   let rec typ_into_elt (typs: typ subst) (l: rem_elt) : rem_elt
   let rec idx_into_expr (sub: idx subst) (x: rem_expr) : rem_expr
   let rec idx_into_elt (sub: idx subst) (l: rem_elt) : rem_elt
*)

let rec typ_into_typ (sub: typ subst) (t: typ) : typ =
  match t with
  | (TFloat | TInt | TBool) as t_ -> t_
  | TDProd (ivars, body) -> TDProd (ivars, typ_into_typ sub body)
  | TDSum (ivars, body) -> TDSum (ivars, typ_into_typ sub body)
  | TFun (ins, out) -> TFun ((List.map ~f:(typ_into_typ sub) ins),
                             typ_into_typ sub out)
  | TArray (shape, elts) -> TArray (shape, typ_into_typ sub elts)
  | TAll (tvars, body)
    -> TAll (tvars,
             typ_into_typ (List.fold ~init:sub
                             ~f:(List.Assoc.remove ~equal:(=)) tvars)
               body)
  | TVar v -> Option.value ~default:(TVar v) (List.Assoc.find sub v)

let rec idx_into_idx (sub: idx subst) (i: idx) : idx =
  match i with
  | INat _ as i_ -> i_
  | IShape idxs -> IShape (List.map ~f:(idx_into_idx sub) idxs)
  | ISum (idx1, idx2) -> ISum (idx_into_idx sub idx1, idx_into_idx sub idx2)
  | IVar v -> Option.value ~default:(IVar v) (List.Assoc.find sub v)

let rec idx_into_typ (sub: idx subst) (t: typ) : typ =
  match t with
  | TFloat | TInt | TBool as t_ -> t_
  | TDProd (ivars, body)
    -> TDProd (ivars, (idx_into_typ
                         (List.fold ~init:sub ~f:(List.Assoc.remove ~equal:(=))
                            (List.map ~f:fst ivars))
                         body))
  | TDSum (ivars, body)
    -> TDSum (ivars, (idx_into_typ
                        (List.fold ~init:sub ~f:(List.Assoc.remove ~equal:(=))
                           (List.map ~f:fst ivars))
                        body))
  | TFun (ins, out) -> TFun ((List.map ~f:(idx_into_typ sub) ins),
                             idx_into_typ sub out)
  | TArray (shape, elts) -> TArray (idx_into_idx sub shape, idx_into_typ sub elts)
  | TAll (tvars, body) -> TAll (tvars, idx_into_typ sub body)
  | TVar v as tv -> tv

let rec idx_into_idx (sub: idx subst) (i: idx) : idx =
  match i with
  | INat n as i_ -> i_
  | IShape idxs -> IShape (List.map ~f:(idx_into_idx sub) idxs)
  | ISum (idx1, idx2) -> ISum (idx_into_idx sub idx1, idx_into_idx sub idx2)
  | IVar v -> Option.value ~default:(IVar v) (List.Assoc.find sub v)

