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
open Annotation
open Typechecker

type app_frame =
| AppFrame of idx list
| NotApp

(* Annotate an AST with the frame shape of each application form. *)
let rec annot_expr_app_frame
    ((AnnRExpr (node_type, expr)): typ ann_expr) : app_frame ann_expr =
  match expr with
  (* TODO: broaden this case to match nested arrays of functions *)
  | App ((AnnRExpr (fn_position_typ, _) as fn_expr),
         args)->
    let fn_typ = elt_of_typ fn_position_typ in
    let ret_type =
      (match fn_typ with
      | Some (TFun (_, ret)) -> ret
      (* The element type of the array in function position is not a function
         type (should not happen in well-typed AST). *)
      | _ -> assert false) in
    (match frame_contribution ret_type node_type with
    | Some idxs -> AnnRExpr (AppFrame idxs,
                             App (annot_expr_app_frame fn_expr,
                                  List.map ~f:annot_expr_app_frame args))
    (* We have an app form whose type is not a frame around its function's
       return type (should not happen in well-typed AST). *)
    | None -> assert false)
  | _ -> AnnRExpr (NotApp, (map_expr_form
                              ~f_expr:annot_expr_app_frame
                              ~f_elt:annot_elt_app_frame
                              expr))
and annot_elt_app_frame
    ((AnnRElt (_, elt)): typ ann_elt) : app_frame ann_elt =
  AnnRElt (NotApp, map_elt_form ~f_expr:annot_expr_app_frame elt)
let annot_defn_app_frame
    ((AnnRDefn (n, t, e)): typ ann_defn) : app_frame ann_defn =
  AnnRDefn (n, t, annot_expr_app_frame e)
let annot_prog_app_frame
    ((AnnRProg (_, defns, expr)): typ ann_prog) : app_frame ann_prog =
  let new_expr = annot_expr_app_frame expr in
  let new_annot = annot_of_expr new_expr in
  AnnRProg (new_annot,
            List.map ~f:annot_defn_app_frame defns,
            new_expr)


type arg_frame =
| ArgFrame of idx list
| NotArg

(* Annotate subnodes of function application nodes with their argument frame
   shapes. We have to track whether we're being called on a node that is
   directly part of an application form (e.g., function position in a type/index
   applciation or the body of an abstraction form) and if so, what enclosing app
   form's principal frame is. If we are currently at an argument, we note its
   contribution to the application's frame. Otherwise, we mark it as NotArg. *)
let rec annot_expr_arg_frame
    ((AnnRExpr (node_type, expr)): typ ann_expr)
    ~(outer_expectation: typ option) : arg_frame ann_expr =
  let my_frame = match outer_expectation with
    | Some t -> ArgFrame (Option.value_exn (frame_contribution t node_type))
    | None -> NotArg in
  match expr with
  | App ((AnnRExpr (fn_type, fn_expr_form)) as fn, args) ->
    let arg_expected_typs = match elt_of_typ fn_type with
      | (Some (TFun (typs, _))) -> typs
      (* In a well-typed AST, the array in function position should have
         functions as its elements. *)
      | _ -> assert false in
    AnnRExpr (my_frame, App (annot_expr_arg_frame
                               ~outer_expectation:(elt_of_typ fn_type) fn,
                             List.map2_exn
                               ~f:(fun expect arg ->
                                 annot_expr_arg_frame
                                     ~outer_expectation:(Some expect) arg)
                               arg_expected_typs args))
  | _ -> AnnRExpr (my_frame, (map_expr_form
                                ~f_expr:(annot_expr_arg_frame
                                           ~outer_expectation:None)
                                ~f_elt:annot_elt_arg_frame expr))

and annot_elt_arg_frame
    ((AnnRElt (node_type, elt)): typ ann_elt) : arg_frame ann_elt =
  match elt with
  | Expr e -> AnnRElt (NotArg,
                       Expr (annot_expr_arg_frame ~outer_expectation:None e))
  | Lam (bindings, body) ->
    AnnRElt (NotArg, Lam (bindings,
                          annot_expr_arg_frame ~outer_expectation:None body))
  | Float _ | Int _ | Bool _ as l -> AnnRElt (NotArg, l)

let annot_defn_arg_frame
    ((AnnRDefn (n, t, e)): typ ann_defn) : arg_frame ann_defn =
  AnnRDefn (n, t, annot_expr_arg_frame ~outer_expectation:None e)

let annot_prog_arg_frame
    ((AnnRProg (annot, defns, expr)): typ ann_prog) : arg_frame ann_prog =
  let (AnnRExpr (new_annot, _)) as new_expr =
    annot_expr_arg_frame ~outer_expectation:None expr
  and new_defns = List.map ~f:annot_defn_arg_frame defns in
  AnnRProg (new_annot, new_defns, new_expr)
