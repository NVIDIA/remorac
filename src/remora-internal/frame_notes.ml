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
with sexp

let app_frame_of_option = function
  | Some dims -> AppFrame dims | None -> NotApp
let option_of_app_frame = function
  | AppFrame dims -> Some dims | NotApp -> None
let idxs_of_app_frame_exn e = match e with
  | AppFrame idxs -> idxs
  | _ -> raise (Failure ((e |> sexp_of_app_frame |> string_of_sexp)
                         ^ " is not an App form"))

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


type arg_frame_rec = {frame: idx list; expansion: idx list} with sexp
type arg_frame =
| ArgFrame of arg_frame_rec
| NotArg
with sexp

let arg_frame_of_option = function
  | Some dims -> ArgFrame dims | None -> NotArg
let option_of_arg_frame = function
  | ArgFrame dims -> Some dims | NotArg -> None
let frame_of_arg_exn = function
  | ArgFrame {frame = f; expansion = _} -> f
  | NotArg -> raise (Failure "Not an arg")
let expansion_of_arg_exn = function
  | ArgFrame {frame = _; expansion = e} -> e
  | NotArg -> raise (Failure "Not an arg")



(* Annotate subnodes of function application nodes with their argument frame
   shapes. We have to track whether we're being called on a node that is
   directly part of an application form (e.g., function position in a type/index
   applciation or the body of an abstraction form) and if so, what enclosing app
   form's principal frame is. If we are currently at an argument, we note how
   its frame must expand in order to match the application's frame. Otherwise,
   we mark it as NotArg. *)
let rec annot_expr_arg_expansion
    ((AnnRExpr ((node_app_frame, node_type), expr)): (app_frame * typ) ann_expr)
    ~(outer_expectation: typ option)
    ~(outer_frame : app_frame) : arg_frame ann_expr =
  let open Option.Monad_infix in
  let my_expansion : arg_frame =
    (option_of_app_frame outer_frame >>= fun outer_frame_shape ->
     outer_expectation >>= fun outer_t ->
     frame_contribution outer_t node_type >>= fun my_frame ->
     shape_drop my_frame outer_frame_shape >>= fun missing_dims ->
     Option.return (ArgFrame {expansion = missing_dims; frame = my_frame}))
        |> (Option.value ~default:NotArg) in
  match expr with
  | App ((AnnRExpr ((_, fn_type), _)) as fn, args) ->
    let arg_expected_typs = match elt_of_typ fn_type with
      | (Some (TFun (typs, _))) -> typs
      (* In a well-typed AST, the array in function position should have
         functions as its elements. *)
      | _ -> assert false in
    AnnRExpr (my_expansion, App (annot_expr_arg_expansion
                                   ~outer_expectation:(elt_of_typ fn_type)
                                   ~outer_frame:node_app_frame fn,
                                 List.map2_exn
                                   ~f:(fun expect arg ->
                                     annot_expr_arg_expansion
                                       ~outer_expectation:(Some expect)
                                       ~outer_frame:node_app_frame arg)
                                   arg_expected_typs args))
  | _ -> AnnRExpr (my_expansion, (map_expr_form
                                    ~f_expr:(annot_expr_arg_expansion
                                               ~outer_expectation:None
                                               ~outer_frame:NotApp)
                                    ~f_elt:annot_elt_arg_expansion expr))

and annot_elt_arg_expansion
    ((AnnRElt (_, elt)): (app_frame * typ) ann_elt)
    : arg_frame ann_elt =
  match elt with
  | Expr e -> AnnRElt (NotArg,
                       Expr (annot_expr_arg_expansion
                               ~outer_expectation:None
                               ~outer_frame:NotApp e))
  | Lam (bindings, body) ->
    AnnRElt (NotArg, Lam (bindings,
                          annot_expr_arg_expansion
                            ~outer_expectation:None
                            ~outer_frame:NotApp body))
  | Float _ | Int _ | Bool _ as l -> AnnRElt (NotArg, l)

let annot_defn_arg_expansion
    ((AnnRDefn (n, t, e)): (app_frame * typ) ann_defn) : arg_frame ann_defn =
  AnnRDefn (n, t, annot_expr_arg_expansion
    ~outer_expectation:None
    ~outer_frame:NotApp e)

let annot_prog_arg_expansion
    ((AnnRProg (_, defns, expr)): (app_frame * typ) ann_prog)
    : arg_frame ann_prog =
  let (AnnRExpr (new_annot, _)) as new_expr =
    annot_expr_arg_expansion
      ~outer_expectation:None
      ~outer_frame:NotApp
      expr
  and new_defns = List.map ~f:annot_defn_arg_expansion defns in
  AnnRProg (new_annot, new_defns, new_expr)

module Passes : sig
  val prog : typ ann_prog -> (typ * arg_frame * app_frame) ann_prog
  val defn : typ ann_defn -> (typ * arg_frame * app_frame) ann_defn
  val expr : typ ann_expr -> (typ * arg_frame * app_frame) ann_expr
  val elt : typ ann_elt -> (typ * arg_frame * app_frame) ann_elt

  val prog_all : rem_prog -> (typ * arg_frame * app_frame) ann_prog option
  val defn_all : rem_defn -> (typ * arg_frame * app_frame) ann_defn option
  val expr_all : rem_expr -> (typ * arg_frame * app_frame) ann_expr option
  val elt_all : rem_elt -> (typ * arg_frame * app_frame) ann_elt option
end = struct
  open Annotation
  open Option.Monad_infix
  let triple x (y, z) = (x, y, z)
  let prog typ_ast =
    let app_ast = annot_prog_app_frame typ_ast in
    let app_typ_ast = Option.value_exn
      ~message:"Failed to merge type and app-frame annotations"
      (annot_prog_merge Tuple2.create app_ast typ_ast) in
    let arg_ast = annot_prog_arg_expansion app_typ_ast in
    let arg_app_ast = Option.value_exn
      ~message:"Failed to merge app-frame and arg-expansion annotations"
      (annot_prog_merge Tuple2.create arg_ast app_ast) in
    Option.value_exn
      ~message:"Failed to merge typ and app/arg annotations"
      (annot_prog_merge triple typ_ast arg_app_ast)
  let prog_all ast =
    ast |> Typechecker.Passes.prog_all >>| prog

  let defn typ_ast =
    let app_ast = annot_defn_app_frame typ_ast in
    let app_typ_ast = Option.value_exn
      ~message:"Failed to merge type and app-frame annotations"
      (annot_defn_merge Tuple2.create app_ast typ_ast) in
    let arg_ast = annot_defn_arg_expansion app_typ_ast in
    let arg_app_ast = Option.value_exn
      ~message:"Failed to merge app-frame and arg-expansion annotations"
      (annot_defn_merge Tuple2.create arg_ast app_ast) in
    Option.value_exn
      ~message:"Failed to merge typ and app/arg annotations"
      (annot_defn_merge triple typ_ast arg_app_ast)
  let defn_all ast =
    ast |> Typechecker.Passes.defn_all >>| defn

  let expr typ_ast =
    let app_ast = annot_expr_app_frame typ_ast in
    let app_typ_ast = Option.value_exn
        ~message:"Failed to merge type and app-frame annotations"
      (annot_expr_merge Tuple2.create app_ast typ_ast) in
    let arg_ast = annot_expr_arg_expansion
      ~outer_expectation:None
      ~outer_frame:NotApp
      app_typ_ast in
    let arg_app_ast = Option.value_exn
      ~message:"Failed to merge app-frame and arg-expansion annotations"
      (annot_expr_merge Tuple2.create arg_ast app_ast) in
    Option.value_exn
      ~message:"Failed to merge typ and app/arg annotations"
      (annot_expr_merge triple typ_ast arg_app_ast)
  let expr_all ast =
    ast |> Typechecker.Passes.expr_all >>| expr

  let elt typ_ast =
    let app_ast = annot_elt_app_frame typ_ast in
    let app_typ_ast = Option.value_exn
        ~message:"Failed to merge type and app-frame annotations"
      (annot_elt_merge Tuple2.create app_ast typ_ast) in
    let arg_ast = annot_elt_arg_expansion app_typ_ast in
    let arg_app_ast = Option.value_exn
      ~message:"Failed to merge app-frame and arg-expansion annotations"
      (annot_elt_merge Tuple2.create arg_ast app_ast) in
    Option.value_exn
      ~message:"Failed to merge typ and app/arg annotations"
      (annot_elt_merge triple typ_ast arg_app_ast)
  let elt_all ast =
    ast |> Typechecker.Passes.elt_all >>| elt
end
