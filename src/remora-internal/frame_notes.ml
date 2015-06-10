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
| NoApp

(* Annotate an AST with the frame shape of each application form. *)
let rec annot_expr_app_frame ((AnnRExpr (node_type, expr)): typ ann_expr) : app_frame ann_expr =
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
  | _ -> AnnRExpr (NoApp, map_expr_form ~f_expr:annot_expr_app_frame ~f_elt:annot_elt_app_frame expr)
and annot_elt_app_frame ((AnnRElt (_, elt)): typ ann_elt) : app_frame ann_elt =
  AnnRElt (NoApp, map_elt_form ~f_expr:annot_expr_app_frame elt)
let annot_defn_app_frame ((AnnRDefn (n, t, e)): typ ann_defn) : app_frame ann_defn =
  AnnRDefn (n, t, annot_expr_app_frame e)
let annot_prog_app_frame ((AnnRProg (_, defns, expr)): typ ann_prog) : app_frame ann_prog =
  let new_expr = annot_expr_app_frame expr in
  let new_annot = annot_of_expr new_expr in
  AnnRProg (new_annot,
            List.map ~f:annot_defn_app_frame defns,
            new_expr)
