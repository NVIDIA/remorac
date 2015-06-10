(******************************************************************************)
(* Copyright 2015 NVIDIA Corporation.  All rights reserved.                   *)
(*                                                                            *)
(* NOTICE TO USER: The source code, and related code and software             *)
(* ("Code"), is copyrighted under U.S. and international laws.                *)
(*                                                                            *)
(* NVIDIA Corporation owns the copyright and any patents issued or            *)
(* pending for the Code.                                                      *)
(*                                                                            *)
(* NVIDIA CORPORATION MAKES NO REPRESENTATION ABOUT THE SUITABILITY           *)
(* OF THIS CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS-IS" WITHOUT EXPRESS      *)
(* OR IMPLIED WARRANTY OF ANY KIND.  NVIDIA CORPORATION DISCLAIMS ALL         *)
(* WARRANTIES WITH REGARD TO THE CODE, INCLUDING NON-INFRINGEMENT, AND        *)
(* ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR     *)
(* PURPOSE.  IN NO EVENT SHALL NVIDIA CORPORATION BE LIABLE FOR ANY           *)
(* DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES            *)
(* WHATSOEVER ARISING OUT OF OR IN ANY WAY RELATED TO THE USE OR              *)
(* PERFORMANCE OF THE CODE, INCLUDING, BUT NOT LIMITED TO, INFRINGEMENT,      *)
(* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,            *)
(* NEGLIGENCE OR OTHER TORTIOUS ACTION, AND WHETHER OR NOT THE                *)
(* POSSIBILITY OF SUCH DAMAGES WERE KNOWN OR MADE KNOWN TO NVIDIA             *)
(* CORPORATION.                                                               *)
(******************************************************************************)

open Basic_ast
open Typechecker
open Frame_notes
open Core.Option
open Core.Option.Monad_infix
module U = OUnit2;;

module Test_annot_expr_app_frame : sig
  val tests: U.test
end = struct
  open Test_basic_ast
  let test_1 _ =
    U.assert_equal
      ((nested_to_unary_app
           |> annot_expr_init ~init:""
           |> annot_expr_type [][][]
           |> well_typed_of_expr) >>= fun typed_ast ->
       (typed_ast
           |> annot_expr_app_frame
           |> annot_of_expr
           |> return))
      (Some (AppFrame [IShape [INat 3]; IShape [INat 2]]))
  let tests =
    let open OUnit2 in
    "add application frame shape annotation">:::
      ["apply array of functions to scalar">:: test_1]
end

module UnitTests : sig
  val tests: U.test
end = struct
  let tests =
    let open OUnit2 in
    "frame notes tests">:::
      [Test_annot_expr_app_frame.tests]
end
