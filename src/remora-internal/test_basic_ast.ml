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

let test_t_int = TInt
let test_t_float = TFloat


let ta =
AnnRExpr ((TArray (IShape [INat 2], TInt)),
          (Arr ([2], [AnnRElt (TInt, Int 3);
                           AnnRElt (TInt, Int 2)])));;

ta |> sexp_of_ann_expr sexp_of_typ
   |> Sexp.to_string_hum ~indent:4
   |> print_string

let scalar_of_elt e = RExpr (Arr ([], [e]))
let scalar_of_expr e = scalar_of_elt (RElt (Expr e))
let scalar_of_elt_form e = scalar_of_elt (RElt e)
let scalar_of_expr_form e = scalar_of_expr (RExpr e)

let flat_arr_2_3 =
  RExpr (Arr ([2;3], [RElt (Int 4); RElt (Int 1); RElt (Int 6);
                      RElt (Int 2); RElt (Int 3); RElt (Int 5)]))

let flat_arr_0_4 = RExpr (Arr ([0;4], []))

let arr_2 = RExpr (Arr ([2], [RElt (Bool false); RElt (Bool true)]))

let arr_wrong =
  RExpr (Arr ([2;3], [RElt (Int 4); RElt (Int 1); RElt (Int 6);
                      RElt (Int 2); RElt (Int 3)]))

let nest_arr_2_3 =
  RExpr (Arr ([2],
              [RElt (Expr (RExpr (Arr ([3], [RElt (Int 4);
                                             RElt (Int 1);
                                             RElt (Int 6)]))));
               RElt (Expr (RExpr (Arr ([3], [RElt (Int 2);
                                             RElt (Int 3);
                                             RElt (Int 5)]))))]))

let unary_lambda =
  RElt (Lam ([("x", TArray (IShape [], TInt))], scalar_of_elt_form (Int 3)))

let binary_lambda =
  RElt (Lam ([("x", TArray (IShape [INat 3], TFloat));
              ("y", TArray (IShape [INat 1], TBool))],
             scalar_of_elt_form (Int 3)))

let unary_app =
  RExpr (App (RExpr (Arr ([], [unary_lambda])),
              [scalar_of_elt_form (Int 4)]))

let binary_app =
  RExpr (App (RExpr (Arr ([], [binary_lambda])),
              [RExpr (Arr ([3], [RElt (Float 1.0);
                                 RElt (Float 2.0);
                                 RElt (Float 3.0)]));
               RExpr (Arr ([1], [RElt (Bool false)]))]))

let elt_of_expr_form e = RElt (Expr (RExpr e))

let unary_to_nested_app =
  RExpr
    (App
       (RExpr
          (Arr ([],
                [(RElt (Lam ([("x", TArray (IShape [INat 2; INat 3], TInt))],
                             RExpr (Var "x"))))])),
        [RExpr (Arr ([2], [elt_of_expr_form
                              (Arr ([3], [RElt (Int 1);
                                          RElt (Int 2);
                                          RElt (Int 3)]));
                           elt_of_expr_form
                             (Arr ([3], [RElt (Int 4);
                                         RElt (Int 5);
                                         RElt (Int 6)]))]))]))

let nested_to_unary_app =
  RExpr
    (App
       (RExpr
          (Arr ([3],
                [elt_of_expr_form
                    (Arr ([2],
                          [RElt (Lam ([("x", TArray (IShape [], TInt))],
                                      scalar_of_elt_form (Int 1)));
                           RElt (Lam ([("x", TArray (IShape [], TInt))],
                                      scalar_of_elt_form (Int 2)))]));
                 elt_of_expr_form
                   (Arr ([2],
                         [RElt (Lam ([("x", TArray (IShape [], TInt))],
                                     scalar_of_elt_form (Int 3)));
                          RElt (Lam ([("x", TArray (IShape [], TInt))],
                                     scalar_of_elt_form (Int 4)))]));
                 elt_of_expr_form
                   (Arr ([2],
                         [RElt (Lam ([("x", TArray (IShape [], TInt))],
                                     scalar_of_elt_form (Int 5)));
                          RElt (Lam ([("x", TArray (IShape [], TInt))],
                                     scalar_of_elt_form (Int 6)))]))])),
        [RExpr (Arr ([3], [RElt (Int 7); RElt (Int 23523); RElt (Int 245)]))]))

let type_abst =
  RExpr (TLam (["elt"],
               scalar_of_elt_form (Lam ([("x", TArray (IShape [], TVar "elt"))],
                                        RExpr (Var "x")))))

let type_abst_bad =
  RExpr (TLam (["elt"],
               scalar_of_elt_form (Lam ([("x", TArray (IShape [], TVar "foo"))],
                                        RExpr (Var "x")))))

let type_app = RExpr (TApp (type_abst, [TBool]))

let index_abst =
  RExpr (ILam (["d", SNat],
               RExpr (Arr ([],
                           [RElt (Lam ([("l", TArray (IShape [IVar "d"],
                                                      TInt))],
                                       RExpr (Var "l")))]))))

let index_app = RExpr (IApp (index_abst, [INat 6]))

let dep_sum_create =
  RExpr (Pack ([INat 3],
               RExpr (Arr ([3], [RElt (Int 0); RElt (Int 1); RElt (Int 2)])),
               TDSum ([("d", SNat)], TArray (IShape [IVar "d"], TInt))))


let dep_sum_project =
  RExpr (Unpack (["l"], "c", dep_sum_create, scalar_of_elt_form (Int 0)))


let remora_compose =
  let inner_lam =
    RElt (Lam (["x", TArray (IVar "s1", TVar "alpha")],
               RExpr (App (RExpr (Var "g"),
                           [(RExpr (App (RExpr (Var "f"),
                                         [RExpr (Var "x")])))])))) in
  let outer_lam =
    RElt (Lam (["f", TFun ([TArray (IVar "s1", TVar "alpha")],
                           TArray (IVar "s2", TVar "beta"));
                "g", TFun ([TArray (IVar "s2", TVar "beta")],
                           TArray (IVar "s3", TVar "gamma"))],
               scalar_of_elt inner_lam)) in
  let type_lam =
    RExpr (TLam (["alpha"; "beta"; "gamma"],
                 scalar_of_elt outer_lam))  in
  RExpr (ILam (["s1", SShape; "s2", SShape; "s3", SShape], type_lam))


let fork_compose =
  let inner_lam =
    RElt (Lam (["x", TArray (IVar "s-li", TVar "t-li");
                "y", TArray (IVar "s-ri", TVar "t-ri")],
               RExpr (App (RExpr (Var "f-j"),
                           [RExpr (App (RExpr (Var "f-l"),
                                        [RExpr (Var "x")]));
                            RExpr (App (RExpr (Var "f-r"),
                                        [RExpr (Var "y")]))])))) in
  let outer_lam =
    RElt (Lam (["f-l", TArray (IShape [],
                               TFun ([TArray (IVar "s-li", TVar "t-li")],
                                     TArray (IVar "s-lo", TVar "t-lo")));
                "f-r", TArray (IShape [],
                               TFun ([TArray (IVar "s-ri", TVar "t-ri")],
                                     TArray (IVar "s-ro", TVar "t-ro")));
                "f-j", TArray (IShape [],
                               TFun ([TArray (IVar "s-lo", TVar "t-lo");
                                      TArray (IVar "s-ro", TVar "t-ro")],
                                     TArray (IVar "s-jo", TVar "t-jo")))],
               scalar_of_elt inner_lam)) in
  let type_lam =
    RExpr (TLam (["t-li"; "t-lo"; "t-ri"; "t-ro"; "t-jo"],
                 scalar_of_elt outer_lam)) in
  RExpr (ILam (["s-li", SShape; "s-lo", SShape;
                "s-ri", SShape; "s-ro", SShape;
                "s-jo", SShape],
               type_lam))
