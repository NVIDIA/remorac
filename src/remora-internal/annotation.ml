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
open Core.Option
open Core.Option.Monad_infix

(* Not sure why there's only map2_exn in Core and no total map2 *)
let map2 ~f xs ys =
  (try Some (List.map2_exn ~f:f xs ys) with
  | _ -> None)

(* Given two matching ASTs, merge their annotations *)
let rec annot_elt_merge
    (f: 'a -> 'b -> 'c)
    (ast1: 'a ann_elt)
    (ast2: 'b ann_elt) : 'c ann_elt option =
  let (AnnRElt (annot1, elt1), AnnRElt (annot2, elt2)) = (ast1, ast2) in
  let new_annot = f annot1 annot2
  and (new_elt: ('c ann_elt, 'c ann_expr) elt_form option) =
    match (elt1, elt2) with
    | (Lam (bind1, body1), Lam (bind2, body2)) ->
      if (bind1 = bind2)
      then (annot_expr_merge f body1 body2 >>= fun (new_body: 'c ann_expr) ->
            Some (Lam (bind1, new_body)))
      else None
    | (Expr e1, Expr e2) ->
      annot_expr_merge f e1 e2 >>= fun (new_expr: 'c ann_expr) ->
      Some (Expr new_expr)
    (* In these cases, must reconstruct elt1 to use it at a different type. *)
    | ((Float c1) as v1, Float c2) -> Option.some_if (c1 = c2) v1
    | ((Int c1) as v1, Int c2) -> Option.some_if (c1 = c2) v1
    | ((Bool c1) as v1, Bool c2) -> Option.some_if (c1 = c2) v1
    (* Anything else with an already-handled form means mismatching ASTs. *)
    | ((Lam _ | Expr _ | Float _ | Int _ | Bool _), _) -> None
  in new_elt >>= fun valid_new_elt ->
  return (AnnRElt (new_annot, valid_new_elt))
and annot_expr_merge
    (f: 'a -> 'b -> 'c)
    (ast1: 'a ann_expr)
    (ast2: 'b ann_expr) : 'c ann_expr option =
  let (AnnRExpr (annot1, expr1), AnnRExpr (annot2, expr2)) = (ast1, ast2) in
  let new_annot = f annot1 annot2
  and (new_expr: ('c ann_expr, 'c ann_elt) expr_form option) =
    match (expr1, expr2) with
    | (App (fn1, args1), App (fn2, args2)) ->
      annot_expr_merge f fn1 fn2 >>= fun new_fn ->
      map2 ~f:(annot_expr_merge f) args1 args2 >>= fun merged ->
      Option.all merged >>= fun new_args ->
      return (App (new_fn, new_args))
    | (TApp (fn1, t_args1), TApp (fn2, t_args2)) ->
      annot_expr_merge f fn1 fn2 >>= fun new_fn ->
      Option.some_if (t_args1 = t_args2) (TApp (new_fn, t_args1))
    | (TLam (bind1, body1), TLam (bind2, body2)) ->
      if bind1 = bind2
      then annot_expr_merge f body1 body2 >>= fun new_body ->
      Some (TLam (bind1, new_body))
      else None
    | (IApp (fn1, i_args1), IApp (fn2, i_args2)) ->
      annot_expr_merge f fn1 fn2 >>= fun new_fn ->
      Option.some_if (i_args1 = i_args2) (IApp (new_fn, i_args1))
    | (ILam (bind1, body1), ILam (bind2, body2)) ->
      if bind1 = bind2
      then annot_expr_merge f body1 body2 >>= fun new_body ->
      Some (ILam (bind1, new_body))
      else None
    | (Arr (dims1, elts1) , Arr (dims2, elts2)) ->
      if dims1 = dims2
      (* then (try Some (List.map2_exn ~f:annot_elt_merge elts1 elts2) with *)
      (* | Invalid_argument _ -> None) >>= fun merged -> *)
      then map2 ~f:(annot_elt_merge f) elts1 elts2 >>= fun merged ->
      Option.all merged >>= fun new_elts ->
      return (Arr (dims2, new_elts))
      else None
    | (Var v1 as v, Var v2) -> Option.some_if (v1 = v2) v
    | (Pack (idxs1, value1, type1), Pack (idxs2, value2, type2)) ->
      if idxs1 = idxs2 && type1 = type2
      then (annot_expr_merge f value1 value2 >>= fun new_value ->
            Some (Pack (idxs1, new_value, type1)))
      else None
    | (Unpack (i_vars1, v1, dsum1, body1),
       Unpack (i_vars2, v2, dsum2, body2)) ->
      if i_vars1 = i_vars2 && v1 = v2
      then annot_expr_merge f dsum1 dsum2 >>= fun new_dsum ->
      annot_expr_merge f body1 body2 >>= fun new_body ->
      return (Unpack (i_vars1, v1, new_dsum, new_body))
      else None
    | (Let (var1, bound1, body1), Let (var2, bound2, body2)) ->
      if var1 = var2
      then annot_expr_merge f bound1 bound2 >>= fun new_bound ->
      annot_expr_merge f body1 body2 >>= fun new_body ->
      return (Let (var1, new_bound, new_body))
      else None
    | (Tuple elts1, Tuple elts2) ->
      map2 ~f:(annot_expr_merge f) elts1 elts2 |>
          Option.map ~f:Option.all |> Option.join >>= fun new_elts ->
      return (Tuple new_elts)
    | (Field (n1, tup1), Field (n2, tup2)) ->
      if n1 = n2
      then annot_expr_merge f tup1 tup2 >>= fun new_tup ->
      return (Field (n1, new_tup))
      else None
    | (LetTup (vars1, bound1, body1), LetTup (vars2, bound2, body2)) ->
      if vars1 = vars2
      then annot_expr_merge f bound1 bound2 >>= fun new_bound ->
      annot_expr_merge f body1 body2 >>= fun new_body ->
      return (LetTup (vars1, new_bound, new_body))
      else None
    (* Anything else with an already-handled form means mismatching ASTs. *)
    | ((App _ | TApp _ | TLam _ | IApp _ | ILam _
           | Arr _ | Var _ | Pack _ | Unpack _
           | Let _ | Tuple _ | Field _ | LetTup _), _) -> None
  in new_expr >>= fun valid_new_expr ->
  return (AnnRExpr (new_annot, valid_new_expr))
;;
let annot_defn_merge
    (f: 'a -> 'b -> 'c)
    (ast1: 'a ann_defn)
    (ast2: 'b ann_defn) : 'c ann_defn option =
  let (AnnRDefn (n1, t1, b1), AnnRDefn (n2, t2, b2)) = (ast1, ast2) in
  if (n1 = n2 && t1 = t2)
  then annot_expr_merge f b1 b2 >>= fun body -> AnnRDefn (n1, t1, body)
    |> return
  else None
let annot_prog_merge
    (f: 'a -> 'b -> 'c)
    (ast1: 'a ann_prog)
    (ast2: 'b ann_prog) : 'c ann_prog option =
  let (AnnRProg (annot1, defs1, expr1), AnnRProg (annot2, defs2, expr2))
      = (ast1, ast2) in
  map2 ~f:(annot_defn_merge f) defs1 defs2
    >>| Option.all |> Option.join
  >>= fun (ds: 'c ann_defn list) ->
  annot_expr_merge f expr1 expr2 >>= fun e ->
  AnnRProg (f annot1 annot2, ds, e) |> return


(* Given an annotated AST, apply a function to its annotations *)
let rec annot_elt_fmap
    ~(f: 'a -> 'b)
    (AnnRElt (annot, elt): 'a ann_elt) : 'b ann_elt =
  AnnRElt (f annot, (map_elt_form ~f_expr:(annot_expr_fmap ~f:f) elt))
and annot_expr_fmap
    ~(f: 'a -> 'b)
    (AnnRExpr (annot, expr): 'a ann_expr) : 'b ann_expr =
AnnRExpr (f annot, (map_expr_form
                      ~f_expr:(annot_expr_fmap ~f:f)
                      ~f_elt:(annot_elt_fmap ~f:f) expr))
;;

let annot_defn_fmap
    ~(f: 'a -> 'b)
    (ast: 'a ann_defn) : 'b ann_defn =
  let AnnRDefn (n, t, v) = ast in AnnRDefn (n, t, annot_expr_fmap ~f:f v)

let annot_prog_fmap
    ~(f: 'a -> 'b)
    (ast: 'a ann_prog) : 'b ann_prog =
  let AnnRProg (annot, defns, expr) = ast in
  AnnRProg (f annot,
            List.map ~f:(annot_defn_fmap ~f:f) defns,
            annot_expr_fmap ~f:f expr)
