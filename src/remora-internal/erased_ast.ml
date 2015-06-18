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

(* AST for the type-erased version of Remora *)

open Core.Std
open Core.Option.Monad_infix
module B = Basic_ast;;

type var = B.var with sexp

type idx = B.idx with sexp

type srt = B.srt with sexp

(* TODO: This TUnknown thing seems wrong. If a type cannot be picked, it should
   probably just be None, and we should use typ option if we may be unable to
   pick a particular type. Conversion from a basic AST will require type
   annotations (not just frame annotations). *)

(* Type information is reduced down to only what is used by the dynamic
   semantics. Universal types are replaced by their bodies. Base types carry no
   information, but it might be useful to give this an int parameter describing
   the size in memory of this data type. Type variables represent unknown types
   and are noted as such. There are no more bindings for those variables to
   refer to. *)
type typ =
| TBase
(* Variables must represent array types (of indeterminate shape). *)
| TVar
| TUnknown
| TFun of (typ list * typ)
| TDProd of ((var * srt) list * typ)
| TDSum of ((var * srt) list * typ)
| TArray of (idx * typ)
with sexp
let rec of_typ (t: B.typ) : typ =
  match t with
  | B.TFloat | B.TInt | B.TBool -> TBase
  | B.TVar _ -> TVar
  | B.TDProd (bindings, body) -> TDProd (bindings, of_typ body)
  | B.TDSum (bindings, body) -> TDSum (bindings, of_typ body)
  | B.TFun (args, ret) -> TFun (List.map ~f:of_typ args, of_typ ret)
  | B.TAll (_, body) -> of_typ body
  | B.TArray (shp, elt) -> TArray (shp, of_typ elt)

let rec shape_of_typ (t: typ) =
  match t with
  | TArray (s, t) ->
    Typechecker.expand_shape s >>= fun s_ ->
    shape_of_typ t >>= fun t_ ->
    List.append s_ t_ |> Option.return
  | _ -> None

(* TODO: It may be better to just roll the annotation field into this structure
   instead of delaying the recursion and adding it there.
   It's unlikely that I'll want an un-annotated version of the erased AST at any
   point during compilation, but it might be easier to inject test data. *)
type ('self_t, 'elt_t) expr_form =
| App of ('self_t * 'self_t list * typ)
| IApp of ('self_t * idx list)
| ILam of ((var * srt) list * 'self_t)
| Arr of (int list * 'elt_t list)
| Var of var
| Pack of (idx list * 'self_t)
| Unpack of (var list * var * 'self_t * 'self_t)
and ('self_t, 'expr_t) elt_form =
| Float of float
| Int of int
| Bool of bool
| Lam of (var list * 'expr_t)
| Expr of 'expr_t
with sexp

let map_expr_form
    ~(f_expr: 'old_self_t -> 'new_self_t)
    ~(f_elt: 'old_elt_t -> 'new_elt_t)
    (e: ('old_self_t, 'old_elt_t) expr_form)
    : ('new_self_t, 'new_elt_t) expr_form =
match e with
| App (fn, args, t_decl) -> App (f_expr fn, List.map ~f:f_expr args, t_decl)
| IApp (fn, idxs) -> IApp (f_expr fn, idxs)
| ILam (bindings, body) -> ILam (bindings, f_expr body)
| Arr (dims, elts) -> Arr (dims, List.map ~f:f_elt elts)
| Var _ as v -> v
| Pack (idxs, value) -> Pack (idxs, f_expr value)
| Unpack (ivars, v, dsum, body) -> Unpack (ivars, v, f_expr dsum, f_expr body)

let map_elt_form
    ~(f_expr: 'old_expr_t -> 'new_expr_t)
    (l: ('old_self_t, 'old_expr_t) elt_form)
    : ('new_self_t, 'new_expr_t) elt_form =
match l with
| Float _ | Int _ | Bool _ as c -> c
| Lam (bindings, body) -> Lam (bindings, f_expr body)
| Expr e -> Expr (f_expr e)

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


let rec of_expr (B.RExpr e) =
  match e with
  | B.App (fn, args) -> EExpr (App (of_expr fn,
                           List.map ~f:of_expr args,
                           TUnknown))
  | B.TApp (fn, _) -> of_expr fn
  | B.TLam (_, body) -> of_expr body
  | B.IApp (fn, args) -> EExpr (IApp (of_expr fn, args))
  | B.ILam (bindings, body) -> EExpr (ILam (bindings, of_expr body))
  | B.Arr (dims, elts) -> EExpr (Arr (dims, List.map ~f:of_elt elts))
  | B.Var v -> EExpr (Var v)
  | B.Pack (idxs, value, t_decl) -> EExpr (Pack (idxs, of_expr value))
  | B.Unpack (ivars, v, dsum, body) ->
    EExpr (Unpack (ivars, v, of_expr dsum, of_expr body))
and of_elt (B.RElt l) =
  EElt (
    match l with
    | B.Float f -> Float f
    | B.Int i -> Int i
    | B.Bool b -> Bool b
    | B.Lam (bindings, body) -> Lam (List.map ~f:fst bindings, of_expr body)
    | B.Expr e -> Expr (of_expr e))
let of_defn (B.RDefn (n, t, v)) =
  EDefn (n, of_typ t, of_expr v)
let of_prog (B.RProg (defns, expr)) =
  EProg (List.map ~f:of_defn defns, of_expr expr)

(* Type-erase an annotated AST. Because type application/abstraction nodes
   are collapsed away by erasure, we need to decide what annotation to use.
   The annotations from from the outer TApp/TLam and the one from the inner
   expression are both available and can be merged using a passed-in
   procedure. In the default case, the outer annotation is kept, and the inner
   annotation is dropped. *)
let rec of_ann_expr
    ?(merge = (fun (a1: 'annot) (a2: 'annot) -> a1))
    (B.AnnRExpr (a, e): 'annot B.ann_expr)
    : 'annot ann_expr =
  match e with
  | B.App (fn, args) ->
    AnnEExpr (a, App (of_ann_expr ~merge:merge fn,
                      List.map ~f:(of_ann_expr ~merge:merge) args,
                      TUnknown))
  | B.TApp (fn, args) ->
    let AnnEExpr (sub_annot, new_node) = of_ann_expr ~merge:merge fn in
    AnnEExpr (merge a sub_annot, new_node)
  | B.TLam (_, body) ->
    let AnnEExpr (sub_annot, new_node) = of_ann_expr ~merge:merge body in
    AnnEExpr (merge a sub_annot, new_node)
  | B.IApp (fn, args) -> AnnEExpr (a, IApp (of_ann_expr ~merge:merge fn, args))
  | B.ILam (bindings, body) ->
    AnnEExpr (a, ILam (bindings, of_ann_expr ~merge:merge body))
  | B.Var v -> AnnEExpr (a, Var v)
  | B.Arr (dims, elts)
    -> AnnEExpr (a, Arr (dims, List.map ~f:(of_ann_elt ~merge:merge) elts))
  | B.Pack (idxs, value, t_decl)
    -> AnnEExpr (a, Pack (idxs, of_ann_expr ~merge:merge value))
  | B.Unpack (ivars, v, dsum, body)
    -> AnnEExpr (a,
                 Unpack (ivars, v,
                         of_ann_expr ~merge:merge dsum,
                         of_ann_expr ~merge:merge body))
and of_ann_elt
    ?(merge = (fun a1 a2 -> a1))
    (B.AnnRElt (a, l): 'annot B.ann_elt)
    : 'annot ann_elt =
  AnnEElt
    (a,
     match l with
     | B.Float f -> Float f
     | B.Int i -> Int i
     | B.Bool b -> Bool b
     | B.Lam (bindings, body) ->
       Lam (List.map ~f:fst bindings, of_ann_expr ~merge:merge body)
     | B.Expr e -> Expr (of_ann_expr ~merge:merge e))
let of_ann_defn ?(merge = (fun a1 a2 -> a1)) (B.AnnRDefn (n, t, v)) =
  AnnEDefn (n, of_typ t, of_ann_expr ~merge:merge v)
let of_ann_prog ?(merge = (fun a1 a2 -> a1)) (B.AnnRProg (annot, defns, expr)) =
  AnnEProg (annot,
            List.map ~f:(of_ann_defn ~merge:merge) defns,
            of_ann_expr ~merge:merge expr)

let rec annot_expr_drop ((AnnEExpr (_, e)): 'annot ann_expr) : erased_expr =
  EExpr (map_expr_form ~f_expr:annot_expr_drop ~f_elt:annot_elt_drop e)
and annot_elt_drop ((AnnEElt (_, e)): 'annot ann_elt) : erased_elt =
  EElt (map_elt_form ~f_expr:annot_expr_drop e)
let annot_defn_drop ((AnnEDefn (n, t, v)): 'annot ann_defn) : erased_defn =
  EDefn (n, t, annot_expr_drop v)
let annot_prog_drop
    ((AnnEProg (_, defns, expr)): 'annot ann_prog) : erased_prog =
  EProg (List.map ~f:annot_defn_drop defns, annot_expr_drop expr)

(* Use AST type annotations to update App forms' result type declarations. *)
let rec fix_expr_app_type (AnnEExpr (t, e): typ ann_expr) : typ ann_expr =
  AnnEExpr (t, (match e with
  | App (fn, args, _) -> App (fn, args, t)
  | _ -> (map_expr_form
            ~f_expr:fix_expr_app_type
            ~f_elt:fix_elt_app_type e)))
and fix_elt_app_type (AnnEElt (t, l): typ ann_elt) : typ ann_elt =
  AnnEElt (t, map_elt_form ~f_expr:fix_expr_app_type l)
let fix_defn_app_type (AnnEDefn (n, t, v): typ ann_defn) : typ ann_defn =
  AnnEDefn (n, t, fix_expr_app_type v)
let fix_prog_app_type (AnnEProg (t, defns, expr): typ ann_prog) : typ ann_prog =
  AnnEProg (t, List.map ~f:fix_defn_app_type defns, fix_expr_app_type expr)

(* Extract the top annotation from an AST. *)
let annot_of_expr ((AnnEExpr (annot, _)): 'a ann_expr) : 'a = annot
let annot_of_elt ((AnnEElt (annot, _)): 'a ann_elt) : 'a = annot
let annot_of_defn ((AnnEDefn (_, _, AnnEExpr (annot, _))): 'a ann_defn) : 'a
    = annot
let annot_of_prog ((AnnEProg (annot, _, _)): 'a ann_prog) : 'a = annot
