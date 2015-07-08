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
open Core.Std
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
           |> Typechecker.Passes.expr_all) >>= fun typed_ast ->
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

(* Some pieces shared by multiple test modules later *)
let typed_lifted_curried_addition =
  Option.value_exn
    (Test_basic_ast.lift_curried_add
        |> annot_expr_init ~init:()
        |> annot_expr_type [] []
            ["c+",
             TArray (IShape [],
                     TFun ([TArray (IShape [], TInt)],
                           TArray (IShape [],
                                   TFun ([TArray (IShape [], TInt)],
                                         TArray (IShape [], TInt)))))]
        |> well_typed_of_expr)
let vec_2 = AnnRExpr (ArgFrame {frame = [IShape [INat 2]];
                                expansion = []},
                      Arr ([2], [AnnRElt (NotArg, Int 10);
                                 AnnRElt (NotArg, Int 20)]))
let mat_2_3 =
  AnnRExpr (ArgFrame {frame = [IShape [INat 2]; IShape [INat 3]];
                      expansion = []},
            Arr ([2; 3],
                 [AnnRElt (NotArg, Int 1); AnnRElt (NotArg, Int 2);
                  AnnRElt (NotArg, Int 3); AnnRElt (NotArg, Int 4);
                  AnnRElt (NotArg, Int 5); AnnRElt (NotArg, Int 6)]))
let annotated_expr =
  AnnRExpr (NotArg,
            App (AnnRExpr (ArgFrame
                             {frame = [IShape [INat 2]];
                              expansion = [IShape [INat 3]]},
                           App (AnnRExpr
                                  (ArgFrame {frame = [IShape []];
                                             expansion = [IShape [INat 2]]},
                                   Var "c+"),
                                [vec_2])),
                 [mat_2_3]))

module Test_annot_expr_arg_expansion : sig
  val tests: U.test
end = struct
  open Test_basic_ast
  let expr_arg_notes base target =
    let app = annot_expr_app_frame base in
    let app_typ_ = (Annotation.annot_expr_merge Tuple2.create app base) in
    let app_typ = match app_typ_ with
        | Some x -> x
        | None -> U.assert_failure
          "+----\n+ could not merge app/typ\n+-----\n" in
    let arg = annot_expr_arg_expansion
      ~outer_frame:NotApp
      ~outer_expectation:None
      app_typ in
    U.assert_equal arg target
  let test_1 _ =
    expr_arg_notes typed_lifted_curried_addition annotated_expr
  let tests =
    let open OUnit2 in
    "add argument frame shape annotations to expr">:::
      ["lifting curried addition">:: test_1]
end

module Test_annot_prog_arg_frame : sig
  val tests: U.test
end = struct
  open Test_basic_ast
  open Typechecker
  let test_1 _ =
    let typed_curried_addition =
      Option.value_exn
        (prog_curried_add
            |> (annot_prog_init ~init:())
            |> (annot_prog_type [] []
                  ["+", TArray (IShape [],
                                TFun ([TArray (IShape [], TInt);
                                       TArray (IShape [], TInt)],
                                      TArray (IShape [], TInt)))])
            |> well_typed_of_prog) in
    let app_expr = AnnRExpr (NotArg,
                             App (AnnRExpr (ArgFrame
                                              {frame = [IShape []];
                                               expansion = []},
                                            Var "+"),
                                  [AnnRExpr (ArgFrame
                                               {frame = [];
                                                expansion = []},
                                             Var "x");
                                   AnnRExpr (ArgFrame
                                               {frame = [];
                                                expansion = []},
                                             Var "y")])) in
    let inner_fun =
      AnnRExpr (NotArg,
                Arr ([], [AnnRElt (NotArg,
                                   Lam ([("y", TArray (IShape [], TInt))],
                                        app_expr))])) in
    let annotated_curried_add =
      AnnRExpr (NotArg, Arr ([],
                             [AnnRElt
                                 (NotArg,
                                  Lam
                                    ([("x", TArray (IShape [], TInt))],
                                     inner_fun))])) in
    let curried_add_type =
      TArray (IShape [],
              TFun ([TArray (IShape [], TInt)],
                    TArray (IShape [],
                            TFun ([TArray (IShape [], TInt)],
                                  TArray (IShape [], TInt))))) in
    let annotated_defn =
      AnnRDefn ("c+", curried_add_type, annotated_curried_add) in
    let test_case = (annot_prog_arg_expansion
                       (Option.value_exn
                          (Annotation.annot_prog_merge
                             Tuple2.create
                             (annot_prog_app_frame typed_curried_addition)
                             typed_curried_addition)))
    and target = (AnnRProg
                    (NotArg,
                     [annotated_defn],
                     annotated_expr)) in
    U.assert_equal test_case target
  let tests =
    let open OUnit2 in
    "add argument frame shape annotations to program">:::
      ["lifting curried addition">:: test_1]
end

module UnitTests : sig
  val tests: U.test
end = struct
  let tests =
    let open OUnit2 in
    "frame notes tests">:::
      [Test_annot_expr_app_frame.tests;
       Test_annot_expr_arg_expansion.tests;
       Test_annot_prog_arg_frame.tests]
end
