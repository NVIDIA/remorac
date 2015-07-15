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
module E = Erased_ast;;
open E
module TB = Test_basic_ast;;
module B = Basic_ast;;
module U = OUnit2;;

let scalar_of_elt e = EExpr (Arr ([], [e]))
let scalar_of_elt_form e = scalar_of_elt (EElt e)

let flat_arr_2_3 =
EExpr (Arr ([2; 3],
            [EElt (Int 4); EElt (Int 1); EElt (Int 6);
             EElt (Int 2); EElt (Int 3); EElt (Int 5)]))

let nest_arr_2_3 =
EExpr
  (Arr ([2],
        [EElt (Expr
                 (EExpr (Arr ([3],
                              [EElt (Int 4); EElt (Int 1); EElt (Int 6)]))));
         EElt (Expr
                 (EExpr (Arr ([3],
                              [EElt (Int 2); EElt (Int 3); EElt (Int 5)]))))]))

let unary_lambda = EElt (Lam (["x"], (EExpr (Arr ([], [EElt (Int 3)])))))

let binary_lambda = EElt (Lam (["x"; "y"],
                               EExpr (Arr ([], [EElt (Int 3)]))))

let unary_app = EExpr (App (EExpr (Arr ([], [unary_lambda])),
                            [EExpr (Arr ([], [EElt (Int 4)]))],
                            TUnknown))

let binary_app = EExpr (App (EExpr (Arr ([], [binary_lambda])),
                             [EExpr (Arr ([3], [EElt (Float 1.0);
                                                EElt (Float 2.0);
                                                EElt (Float 3.0)]));
                              EExpr (Arr ([1], [EElt (Bool false)]))],
                             TUnknown))

let type_abst = EExpr (Arr ([], [EElt (Lam (["x"], EExpr (Var "x")))]))

let index_abst = EExpr (ILam (["d", B.SNat],
                              EExpr (Arr ([],
                                          [EElt (Lam (["l"],
                                                      EExpr (Var "l")))]))))

let index_app = EExpr (IApp (index_abst,
                             [B.INat 6]))

let dep_sum_create =
EExpr (Pack ([B.INat 3],
             EExpr (Arr ([3],
                         [EElt (Int 0); EElt (Int 1); EElt (Int 2)]))))

let dep_sum_project =
EExpr (Unpack (["l"], "c",
               dep_sum_create,
               EExpr (Arr ([], [EElt (Int 0)]))))

let remora_compose =
EExpr
  (ILam
     (["s1", B.SShape; "s2", B.SShape; "s3", B.SShape],
      scalar_of_elt_form
        (Lam (["f"; "g"],
              scalar_of_elt_form
                (Lam (["x"],
                      EExpr
                        (App (EExpr (Var "g"),
                              [EExpr (App (EExpr (Var "f"),
                                           [EExpr (Var "x")],
                                           TUnknown))],
                              TUnknown))))))))

let define_compose =
EDefn ("compose",
       TDProd
         (["s1", B.SShape; "s2", B.SShape; "s3", B.SShape],
          TArray (B.IShape [],
                  TFun
                    ([(TArray (B.IShape [],
                               TFun ([TArray (B.IVar "s1", TVar)],
                                     TArray (B.IVar "s2", TVar))));
                      (TArray (B.IShape [],
                               TFun ([TArray (B.IVar "s2", TVar)],
                                     TArray (B.IVar "s3", TVar))))],
                     TArray (B.IShape [],
                             TFun ([TArray (B.IVar "s1", TVar)],
                                   TArray (B.IVar "s3", TVar)))))),
       remora_compose)

let use_compose =
EExpr (App (EExpr (App (EExpr (IApp (EExpr (Var "compose"),
                                     [B.IShape []; B.IShape []; B.IShape []])),
                        [scalar_of_elt unary_lambda;
                         scalar_of_elt unary_lambda],
                        TUnknown)),
            [scalar_of_elt_form (Int 0)],
            TUnknown))

let prog_compose = EProg ([define_compose], use_compose)

module Test_erasure : sig
  val tests : U.test
end = struct
  let assert_expr_erasure pre post _ =
    U.assert_equal post
      (pre |> B.annot_expr_init ~init:()
           |> E.of_ann_expr ~merge:const
           |> E.annot_expr_drop)
  let assert_elt_erasure pre post _ =
    U.assert_equal post
      (pre |> B.annot_elt_init ~init:()
           |> E.of_ann_elt ~merge:const
           |> E.annot_elt_drop)
  let assert_defn_erasure pre post _ =
    U.assert_equal post
      (pre |> B.annot_defn_init ~init:()
           |> E.of_ann_defn ~merge:const
           |> E.annot_defn_drop)
  let assert_prog_erasure pre post _ =
    U.assert_equal post
      (pre |> B.annot_prog_init ~init:()
           |> E.of_ann_prog ~merge:const
           |> E.annot_prog_drop)
  let tests =
    let open OUnit2 in
        "Generate type annotations, type-erase, drop annotations">:::
        ["flat 2x3">:: assert_expr_erasure TB.flat_arr_2_3 flat_arr_2_3;
         "nested 2x3">:: assert_expr_erasure TB.nest_arr_2_3 nest_arr_2_3;
         "unary lambda">:: assert_elt_erasure TB.unary_lambda unary_lambda;
         "binary lambda">:: assert_elt_erasure TB.binary_lambda binary_lambda;
         "unary app">:: assert_expr_erasure TB.unary_app unary_app;
         "binary app">:: assert_expr_erasure TB.binary_app binary_app;
         (* The TLam and TApp examples should both erase to the same AST *)
         "type abstraction">:: assert_expr_erasure TB.type_abst type_abst;
         "type application">:: assert_expr_erasure TB.type_app type_abst;
         "index abstraction">:: assert_expr_erasure TB.index_abst index_abst;
         "index application">:: assert_expr_erasure TB.index_app index_app;
         "construct dependent sum">::
           assert_expr_erasure TB.dep_sum_create dep_sum_create;
         "destruct dependent sum">::
           assert_expr_erasure TB.dep_sum_project dep_sum_project;
         "composition">::
           assert_expr_erasure TB.remora_compose remora_compose;
         "defining composition">::
           assert_defn_erasure TB.define_compose define_compose;
         "program with composition">::
           assert_prog_erasure TB.prog_compose prog_compose]
end

module UnitTests : sig
  val tests : U.test
end = struct
  let tests =
    let open OUnit2 in
    "erasure tests">:::
      [Test_erasure.tests]
end
