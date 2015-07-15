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
open Core.Option
open Core.Option.Monad_infix
open Frame_notes
module B = Basic_ast;;

type var = B.var with sexp

type idx = B.idx with sexp

type srt = B.srt with sexp

(* TODO: This TUnknown thing seems wrong. If a type cannot be picked, it should
   probably just be None, and we should use typ option if we may be unable to
   pick a particular type. Conversion from a basic AST will require type
   annotations (not just frame annotations).
   Coming back to this later, TUnknown is now used for ascribing a "type" to
   index variables in a later pass. *)

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
| TTuple of typ list
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

(* Build an (erased) array type with a given shape around a given type. *)
let rec typ_of_shape (idxs : idx list) (elt : typ) : typ =
  List.fold_right ~f:(fun i t -> TArray (i, t)) ~init:elt idxs

let rec shape_of_typ (t: typ) =
  match t with
  | TArray (s, t) ->
    Typechecker.expand_shape s >>= fun s_ ->
    shape_of_typ t >>= fun t_ ->
    List.append s_ t_ |> Option.return
  | _ -> None
let rec elt_of_typ (t: typ) : typ option =
  match t with
  | TArray (_, ((TArray (_, t)) as subarray)) -> elt_of_typ subarray
  | TArray (_, t) -> Some t
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
    ?(merge = (fun (parent: 'annot) (child: 'annot) -> parent))
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

(* Merge annotations on two erased ASTs. If App forms disagree about their
   result type declaration, we use the first AST's version. *)
let map2 ~f xs ys =
  (try Some (List.map2_exn ~f:f xs ys) with
  | _ -> None)
let rec annot_expr_merge
    (f: 'a -> 'b -> 'c)
    (AnnEExpr (annot1, expr1): 'a ann_expr)
    (AnnEExpr (annot2, expr2): 'b ann_expr) : 'c ann_expr option =
  let new_annot = f annot1 annot2
  and (new_expr: ('c ann_expr, 'c ann_elt) expr_form option) =
    match (expr1, expr2) with
    (* We are *not* making sure typ1 = typ2. *)
    | (App (fn1, args1, typ1), App (fn2, args2, typ2)) ->
      annot_expr_merge f fn1 fn2 >>= fun new_fn ->
      map2 ~f:(annot_expr_merge f) args1 args2 >>= fun merged ->
      Option.all merged >>= fun new_args ->
      return (App (new_fn, new_args, typ1))
    | (IApp (fn1, args1), IApp (fn2, args2)) ->
      annot_expr_merge f fn1 fn2 >>= fun new_fn ->
      Option.some_if (args1 = args2) (IApp (new_fn, args1))
    | (ILam (bind1, body1), ILam (bind2, body2)) ->
      if bind1 = bind2
      then annot_expr_merge f body1 body2 >>= fun new_body ->
      Some (ILam (bind1, new_body))
      else None
    | (Arr (dims1, elts1), Arr (dims2, elts2)) ->
      if dims1 = dims2
      then map2 ~f:(annot_elt_merge f)
        elts1 elts2 >>= Option.all >>= fun new_elts ->
      return (Arr (dims1, new_elts))
      else None
    | (Var v1 as v, Var v2) -> Option.some_if (v1 = v2) v
    | (Pack (idxs1, value1), Pack (idxs2, value2)) ->
      if (idxs1 = idxs2)
      then annot_expr_merge f value1 value2 >>= fun new_expr ->
      return (Pack (idxs1, new_expr))
      else None
    | (Unpack (ivars1, v1, dsum1, body1), Unpack (ivars2, v2, dsum2, body2)) ->
      if (ivars1 = ivars2 && v1 = v2)
      then annot_expr_merge f dsum1 dsum2 >>= fun new_dsum ->
      annot_expr_merge f body1 body2 >>= fun new_body ->
      return (Unpack (ivars1, v1, new_dsum, new_body))
      else None
    | ((App _ | IApp _ | ILam _ | Arr _ | Var _ | Pack _ | Unpack _), _) -> None
  in
  new_expr >>= fun valid_new_expr ->
  return (AnnEExpr (new_annot, valid_new_expr))
and annot_elt_merge
    (f: 'a -> 'b -> 'c)
    (AnnEElt (annot1, elt1): 'a ann_elt)
    (AnnEElt (annot2, elt2): 'b ann_elt) : 'c ann_elt option =
  let new_annot = f annot1 annot2
  and (new_elt: ('c ann_elt, 'c ann_expr) elt_form option) =
    match (elt1, elt2) with
    | (Float c1 as v1, Float c2) -> Option.some_if (c1 = c2) v1
    | (Int c1 as v1, Int c2) -> Option.some_if (c1 = c2) v1
    | (Bool c1 as v1, Bool c2) -> Option.some_if (c1 = c2) v1
    | (Lam (bind1, body1), Lam (bind2, body2)) ->
      if (bind1 = bind2)
      then annot_expr_merge f body1 body2 >>= fun new_body ->
      return (Lam (bind1, new_body))
      else None
    | (Expr e1, Expr e2) ->
      annot_expr_merge f e1 e2 >>= fun new_expr ->
      return (Expr new_expr)
    | ((Float _ | Int _ | Bool _ | Lam _ | Expr _), _) -> None
  in new_elt >>= fun valid_new_elt ->
  return (AnnEElt (new_annot, valid_new_elt))
let annot_defn_merge
    (f: 'a -> 'b -> 'c)
    ((AnnEDefn (n1, t1, e1)): 'a ann_defn)
    ((AnnEDefn (n2, t2, e2)): 'b ann_defn) : 'c ann_defn option =
  if (n1 = n2 && t1 = t2)
  then annot_expr_merge f e1 e2 >>= fun new_expr ->
  return (AnnEDefn (n1, t1, new_expr))
  else None
let annot_prog_merge
    (f: 'a -> 'b -> 'c)
    ((AnnEProg (annot1, defs1, e1)): 'a ann_prog)
    ((AnnEProg (annot2, defs2, e2)): 'b ann_prog) : 'c ann_prog option =
  map2 ~f:(annot_defn_merge f) defs1 defs2
    >>| Option.all |> Option.join >>= fun new_defs ->
  annot_expr_merge f e1 e2 >>= fun new_expr ->
  return (AnnEProg (f annot1 annot2, new_defs, new_expr))

(* Apply a function to every annotation in an AST. *)
let rec annot_expr_fmap
    ~(f: 'a -> 'b)
    (AnnEExpr (annot, expr): 'a ann_expr) : 'b ann_expr =
  AnnEExpr (f annot, (map_expr_form
                        ~f_expr:(annot_expr_fmap ~f:f)
                        ~f_elt:(annot_elt_fmap ~f:f) expr))
and annot_elt_fmap
    ~(f: 'a -> 'b)
    (AnnEElt (annot, elt): 'a ann_elt) : 'b ann_elt =
  AnnEElt (f annot, map_elt_form ~f_expr:(annot_expr_fmap ~f:f) elt)
let annot_defn_fmap
    ~(f: 'a -> 'b)
    (AnnEDefn (n, t, e): 'a ann_defn) : 'b ann_defn =
  AnnEDefn (n, t, annot_expr_fmap ~f:f e)
let annot_prog_fmap
    ~(f: 'a -> 'b)
    (AnnEProg (annot, defns, expr): 'a ann_prog) : 'b ann_prog =
  AnnEProg (f annot,
            List.map ~f:(annot_defn_fmap ~f:f) defns,
            annot_expr_fmap ~f:f expr)


module Passes : sig
  val prog :
    (B.typ * arg_frame * app_frame) B.ann_prog
    -> (typ * arg_frame * app_frame) ann_prog
  val defn :
    (B.typ * arg_frame * app_frame) B.ann_defn
    -> (typ * arg_frame * app_frame) ann_defn
  val expr :
    (B.typ * arg_frame * app_frame) B.ann_expr
    -> (typ * arg_frame * app_frame) ann_expr
  val elt :
    (B.typ * arg_frame * app_frame) B.ann_elt
    -> (typ * arg_frame * app_frame) ann_elt

  val prog_all : B.rem_prog -> (typ * arg_frame * app_frame) ann_prog option
  val defn_all : B.rem_defn -> (typ * arg_frame * app_frame) ann_defn option
  val expr_all : B.rem_expr -> (typ * arg_frame * app_frame) ann_expr option
  val elt_all : B.rem_elt -> (typ * arg_frame * app_frame) ann_elt option
end = struct
  open Option.Monad_infix
  let prog (ast : (B.typ * arg_frame * app_frame) B.ann_prog)
      : (typ * arg_frame * app_frame) ann_prog =
    (* Generate the type-erased structure, with wrong result type declarations
       on the App forms. *)
    let typ_arg_app_erased = of_ann_prog ast in
    (* Isolate the type annotation on the AST, and translate the Basic_ast.typ
       annotations into Erased_ast.typ annotations. *)
    let typ_erased = annot_prog_fmap
      (fun (t,_,_) -> of_typ t)
      typ_arg_app_erased in
    (* Use the type annotations to produce correct App type declarations. *)
    let typ_fixed = fix_prog_app_type typ_erased in
    (* Merge the version of the AST with fixed App type declarations and the
       fully-annotated version of the AST. *)
    Option.value_exn
      ~message:"Failed to merge annotated and fixed-decls erased-ASTs"
      (annot_prog_merge
         (fun t (_,arg,app) -> (t,arg,app))
         typ_fixed typ_arg_app_erased)
  let prog_all ast = ast |> Frame_notes.Passes.prog_all >>| prog

  let defn ast =
    let typ_arg_app_erased = of_ann_defn ast in
    let typ_erased = annot_defn_fmap
      (fun (t,_,_) -> of_typ t)
      typ_arg_app_erased in
    let typ_fixed = fix_defn_app_type typ_erased in
    Option.value_exn
      ~message:"Failed to merge annotated and fixed-decls erased-ASTs"
      (annot_defn_merge
         (fun t (_,arg,app) -> (t,arg,app))
         typ_fixed typ_arg_app_erased)
  let defn_all ast = ast |> Frame_notes.Passes.defn_all >>| defn

  let expr ast =
    let typ_arg_app_erased = of_ann_expr ast in
    let typ_erased = annot_expr_fmap
      (fun (t,_,_) -> of_typ t)
      typ_arg_app_erased in
    let typ_fixed = fix_expr_app_type typ_erased in
    Option.value_exn
      ~message:"Failed to merge annotated and fixed-decls erased-ASTs"
      (annot_expr_merge
         (fun t (_,arg,app) -> (t,arg,app))
         typ_fixed typ_arg_app_erased)
  let expr_all ast = ast |> Frame_notes.Passes.expr_all >>| expr

  let elt ast =
    let typ_arg_app_erased = of_ann_elt ast in
    let typ_erased = annot_elt_fmap
      (fun (t,_,_) -> of_typ t)
      typ_arg_app_erased in
    let typ_fixed = fix_elt_app_type typ_erased in
    Option.value_exn
      ~message:"Failed to merge annotated and fixed-decls erased-ASTs"
      (annot_elt_merge
         (fun t (_,arg,app) -> (t,arg,app))
         typ_fixed typ_arg_app_erased)
  let elt_all ast = ast |> Frame_notes.Passes.elt_all >>| elt
end
