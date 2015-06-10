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

open Basic_ast
open Typechecker
module U = OUnit2;;

(* Maybe also add tests for
   option_list_xform
   expand_shape
   prefix_max
   build_array_type *)

module Test_env_update : sig
  val tests : U.test
end = struct
  let test_1 (_: U.test_ctxt) =
    U.assert_equal (env_update [] []) []
  let test_2 (_: U.test_ctxt) =
    U.assert_equal (env_update [] ["foo", 4; "bar", 5]) ["foo", 4; "bar", 5]
  let test_3 (_: U.test_ctxt) =
    U.assert_equal (env_update ["foo", 4; "bar", 5] []) ["foo", 4; "bar", 5]
  let test_4 (_: U.test_ctxt) =
    U.assert_equal (env_update ["baz", 2] ["foo", 4; "bar", 5])
      ["baz", 2; "foo", 4; "bar", 5]
  let test_5 (_: U.test_ctxt) =
    U.assert_equal (env_update ["foo", 1; "baz", 2] ["foo", 4; "bar", 5])
      ["foo", 1; "baz", 2; "bar", 5]
  let tests =
    let open OUnit2 in
    "environment update">:::
      ["empty update to empty env">:: test_1;
       "empty update to populated env">:: test_2;
       "populated update to empty env">:: test_3;
       "include new entry">:: test_4;
       "overwrite an existing entry">:: test_5]
end

module Test_srt_of_idx : sig
  val tests : U.test
end = struct
  let test_1 _ =
    U.assert_equal
      (srt_of_idx ["foo", SNat; "bar", SShape] (IVar "bar"))
      (Some SShape)
  let test_2 _ =
    U.assert_equal
      (srt_of_idx ["foo", SNat; "bar", SShape] (IVar "quux"))
      None
  let test_3 _ =
    U.assert_equal
      (srt_of_idx ["foo", SNat; "bar", SShape] (INat 0))
      (Some SNat)
  let test_4 _ =
    U.assert_equal 
      (srt_of_idx ["foo", SNat; "bar", SShape] (ISum (INat 2, IVar "foo")))
      (Some SNat)
  let test_5 _ =
    U.assert_equal
      (srt_of_idx ["foo", SNat; "bar", SShape] (ISum (IVar "bar", INat 2)))
      None
  let test_6 _ =
    U.assert_equal
      (srt_of_idx
         ["foo", SNat; "bar", SShape]
         (IShape [(IVar "foo"); (INat 2)]))
      (Some SShape)
  let test_7 _ =
    U.assert_equal
      (srt_of_idx
         ["foo", SNat; "bar", SShape]
         (IShape [(IVar "bar"); (INat 2)]))
      None
  let tests =
    let open OUnit2 in
    "sort checking">:::
      ["lookup bound var">:: test_1;
       "lookup free var">:: test_2;
       "natural">:: test_3;
       "adding naturals">:: test_4;
       "addition with a non-nat">:: test_5;
       "shape made with naturals">:: test_6;
       "shape made with a non-nat">:: test_7]
end

module Test_kind_of_typ : sig
  val tests : U.test
end = struct
  let test_1 _ =
    U.assert_equal
      (kind_of_typ [] [] (TAll (["x";"y"], TVar "y")))
      (Some ())
  let test_2 _ =
    U.assert_equal
      (kind_of_typ [] ["x", ();"y", ()] (TVar "z"))
      None
  let test_3 _ =
    U.assert_equal
      (kind_of_typ [] []
         (TDProd (["dims", SShape], TFun ([TArray (IVar "dims", TInt)],
                                          TArray (IShape [], TBool)))))
      (Some ())
  let test_4 _ =
    U.assert_equal
      (kind_of_typ [] []
         (TDProd (["len", SNat], TFun ([TArray (IVar "len", TInt)],
                                       TArray (IShape [], TBool)))))
      None
  let test_5 _ =
    U.assert_equal
      (kind_of_typ [] []
         (TDSum (["len", SNat], TArray (IShape [IVar "len"], TFloat))))
      (Some ())
  let test_6 _ =
    U.assert_equal
      (kind_of_typ [] []
         (TDSum (["dims", SShape], TArray (IShape [IVar "dims"], TFloat))))
      None
  let test_7 _ =
    U.assert_equal
      (kind_of_typ [] [] (TFun ([TArray (IVar "dims", TInt)],
                                TArray (IShape [], TBool))))
      None
  let test_8 _ =
    U.assert_equal
      (kind_of_typ
         ["width", SNat; "dims", SShape]
         ["x", (); "y", ()]
         (TArray (IShape [IVar "width"; INat 5], TVar "x")))
      (Some ())
  let test_9 _ =
    U.assert_equal
      (kind_of_typ
         ["width", SNat; "dims", SShape]
         ["x", (); "y", ()]
         (TArray (IShape [INat 3; IVar "length"], TVar "x")))
      None
  let test_10 _ =
    U.assert_equal
      (kind_of_typ
         ["width", SNat; "dims", SShape]
         ["x", (); "y", ()]
         (TArray (IShape [IVar "width"; INat 5], TVar "z")))
      None
  let tests =
    let open OUnit2 in
    "kind checking">:::
      ["forall-bind a typ var">:: test_1;
       "free type var">:: test_2;
       "Pi-bind an idx var">:: test_3;
       "misuse Pi-bound idx var">:: test_4;
       "Sigma-bind an idx var">:: test_5;
       "misuse Sigma-bound idx var">:: test_6;
       "function with ill-formed pieces">:: test_7;
       "well-formed array">:: test_8;
       "array with free idx var">:: test_9;
       "array with free typ var">:: test_10]
end

module Test_uniq_typ : sig
  val tests: U.test
end = struct
  let test_1 _ = U.assert_equal (uniq_typ []) None
  let test_2 _ = U.assert_equal (uniq_typ [TInt]) (Some TInt)
  let test_3 _ = U.assert_equal (uniq_typ [TInt; TInt; TInt]) (Some TInt)
  let test_4 _ = U.assert_equal (uniq_typ [TInt; TBool; TInt]) None
  let tests = 
    let open OUnit2 in
    "selecting only distinct element in a list">:::
      ["empty list">:: test_1;
       "singleton list">:: test_2;
       "uniform list">:: test_3;
       "non-uniform list">:: test_4]
end

module Test_shape_of_typ : sig
  val tests: U.test
end = struct
  let test_1 _ =
    U.assert_equal
      (shape_of_typ (TArray (IShape [], TFloat)))
      (Some [])
  let test_2 _ =
    U.assert_equal
      (shape_of_typ (TArray (IShape [INat 3], TFloat)))
      (Some [IShape [INat 3]])
  let test_3 _ =
    U.assert_equal
      (shape_of_typ (TArray (IShape [INat 3; INat 4], TInt)))
      (Some [IShape [INat 3]; IShape [INat 4]])
  let test_4 _ =
    U.assert_equal
      (shape_of_typ (TArray (IShape [INat 3], TArray (IShape [INat 4], TBool))))
      (Some [IShape [INat 3]; IShape [INat 4]])
  let test_5 _ =
    U.assert_equal
      (shape_of_typ (TArray (IShape [INat 3],
                             TArray (IVar "q",
                                     TArray (IShape [INat 2; INat 4],
                                             TInt)))))
      (Some [IShape [INat 3]; IVar "q"; IShape [INat 2]; IShape [INat 4]])
  let test_6 _ =
    U.assert_equal
      (shape_of_typ (TArray (IShape [INat 8], TArray (IShape [], TBool))))
      (Some [IShape [INat 8]])
  let tests =
    let open OUnit2 in
    "extract shape part of a type">:::
      ["scalar">:: test_1;
       "vector">:: test_2;
       "flat matrix">:: test_3;
       "nested matrix">:: test_4;
       "vector of unknown of matrix">:: test_5;
       "vector of scalar">:: test_6]
end

module Test_elt_of_typ : sig
  val tests: U.test
end = struct
  let test_1 _ =
    U.assert_equal
      (elt_of_typ (TArray (IShape [], TFloat)))
      (Some TFloat)
  let test_2 _ =
    U.assert_equal
      (elt_of_typ (TArray (IShape [INat 4; INat 4], TFun ([], TVar "output"))))
      (Some (TFun ([], TVar "output")))
  let test_3 _ =
    U.assert_equal
      (elt_of_typ (TArray (IShape [INat 4],
                           TArray (IShape [INat 4],
                                   TVar "element"))))
      (Some (TVar "element"))
  let test_4 _ =
    U.assert_equal
      (elt_of_typ (TDSum (["width", SNat],
                          TArray (IShape [IVar "width"; INat 3], TFloat))))
      None
  let tests =
    let open OUnit2 in
    "extract array element part of a type">:::
      ["scalar">:: test_1;
       "flat matrix">:: test_2;
       "nested matrix">:: test_3;
       "dependent sum">:: test_4]
end

module Test_canonicalize_typ : sig
  val tests: U.test
end = struct
  let test_1 _ = U.assert_equal (canonicalize_typ TFloat) (Some TFloat)
  let test_2 _ =
    U.assert_equal
      (canonicalize_typ (TArray (IShape [], TInt)))
      (Some (TArray (IShape [], TInt)))
  let test_3 _ =
    U.assert_equal
      (canonicalize_typ (TArray (IShape [INat 4], TFloat)))
      (Some (TArray (IShape [INat 4], TFloat)))
  let test_4 _ =
    U.assert_equal
      (canonicalize_typ (TArray (IShape [INat 4; INat 2], TFloat)))
      (Some (TArray (IShape [INat 4], TArray (IShape [INat 2], TFloat))))
  let test_5 _ =
    U.assert_equal
      (canonicalize_typ (TArray (IShape [INat 3],
                                 (TArray (IVar "dims", TBool)))))
      (Some (TArray (IShape [INat 3], (TArray (IVar "dims", TBool)))))
  let test_6 _ =
    U.assert_equal
      (canonicalize_typ
         (TArray (IShape [],
                  TFun ([TArray (IShape [INat 2; INat 3], TInt)],
                        TArray (IShape [INat 4; INat 6], TInt)))))
      (Some (TArray (IShape [],
                     TFun ([TArray (IShape [INat 2],
                                    TArray (IShape [INat 3], TInt))],
                           TArray (IShape [INat 4],
                                   TArray (IShape [INat 6], TInt))))))
  let test_7 _ =
    U.assert_equal
      (canonicalize_typ
         (TDProd (["len", SNat],
                  TFun ([TArray (IShape [INat 3; INat 2], TBool)],
                        TArray (IShape [INat 6],
                                TArray (IShape [IVar "len"],
                                        TInt))))))
      (Some (TDProd (["len", SNat],
                     TFun ([TArray (IShape [INat 3],
                                    TArray (IShape [INat 2], TBool))],
                           TArray (IShape [INat 6],
                                   TArray (IShape [IVar "len"],
                                           TInt))))))
  let test_8 _ =
    U.assert_equal
      (canonicalize_typ (TDSum (["outers", SShape],
                                TArray (IVar "outers",
                                        TArray (IShape [INat 8; INat 4],
                                                TFloat)))))
      (Some (TDSum (["outers", SShape],
                    TArray (IVar "outers",
                            TArray (IShape [INat 8],
                                    TArray (IShape [INat 4], TFloat))))))
  let test_9 _ =
    U.assert_equal
      (canonicalize_typ (TAll (["elts"],
                               (TFun ([TArray (IShape [IVar "d1"; IVar "d2"],
                                               TVar "elts")],
                                      TArray (IShape [INat 3], TBool))))))
      (Some (TAll (["elts"],
                   (TFun ([TArray (IShape [IVar "d1"],
                                   TArray (IShape [IVar "d2"], TVar "elts"))],
                          TArray (IShape [INat 3], TBool))))))
  let test_10 _ =
    U.assert_equal
      (canonicalize_typ
         (TArray (IVar "s", TArray (IShape [], TArray (IShape [], TInt)))))
      (Some (TArray (IVar "s", TInt)))
  let test_11 _ =
    U.assert_equal
      (canonicalize_typ
         (TArray (IShape [], TArray (IShape [], TArray (IVar "s", TInt)))))
      (Some (TArray (IVar "s", TInt)))
  let test_12 _ =
    U.assert_equal
      (canonicalize_typ
         (TArray (IShape [], TArray (IVar "s", TArray (IShape [], TInt)))))
      (Some (TArray (IVar "s", TInt)))
  let tests =
    let open OUnit2 in
    "check type equality">:::
      ["atom">:: test_1;
       "scalar">:: test_2;
       "vector">:: test_3;
       "matrix">:: test_4;
       "vector of unknown">:: test_5;
       "matrix->matrix">:: test_6;
       "dependent product">:: test_7;
       "dependent sum">:: test_8;
       "forall">:: test_9;
       "unkown of scalar of scalar">:: test_10;
       "scalar of unknown of scalar">:: test_11;
       "scalar of scalar of unknown">:: test_12]
end

module Test_typ_equal : sig
  val tests: U.test
end = struct
  let test_eq t1 t2 = U.assert_equal true (typ_equal t1 t2)
  let test_neq t1 t2 = U.assert_equal false (typ_equal t1 t2)
  let test_1 _ = test_eq TInt TInt
  let test_2 _ = test_neq TInt TFloat
  let test_3 _ =
    test_eq
      (TDProd (["s1", SShape],
               TFun ([TArray (IVar "s1", TBool)],
                     TArray (IShape [], TBool))))
      (TDProd (["s2", SShape],
               TFun ([TArray (IVar "s2", TBool)],
                     TArray (IShape [], TBool))))
  let test_4 _ =
    test_neq
      (TDProd (["s1", SShape],
               TFun ([TArray (IVar "s1", TBool)],
                     TArray (IShape [], TBool))))
      (TDProd (["s1", SShape],
               TFun ([TArray (IShape [], TBool)],
                     TArray (IShape [], TBool))))
  let test_5 _ =
    test_neq
      (TDProd (["s1", SShape],
               TFun ([TArray (IVar "s1", TBool)],
                     TArray (IShape [], TBool))))
      (TDProd (["s1", SShape],
               TFun ([TArray (IVar "s1", TInt)],
                     TArray (IShape [], TInt))))
  let test_6 _ =
    test_eq
      (TDSum (["l", SNat], TArray (IShape [IVar "l"], TInt)))
      (TDSum (["m", SNat], TArray (IShape [IVar "m"], TInt)))
  let test_7 _ =
    test_neq
      (TDSum (["l", SNat], TArray (IShape [IVar "l"], TInt)))
      (TDSum (["l", SNat], TArray (IShape [IVar "l"], TBool)))
  let test_8 _ =
    test_neq
      (TDSum (["l", SNat], TArray (IShape [IVar "l"], TInt)))
      (TDSum (["l", SNat], TArray (IShape [IVar "l"; INat 3], TInt)))
  let test_9 _ =
    test_eq
      (TAll (["t"], TFun ([TVar "t"], TBool)))
      (TAll (["s"], TFun ([TVar "s"], TBool)))
  let test_10 _ =
    test_neq
      (TAll (["t"], TFun ([TVar "t"], TBool)))
      (TAll (["t"], TFun ([TVar "t"], TFloat)))
  let test_11 _ =
    test_eq
      (TArray (IShape [IVar "i"; IVar "j"], TInt))
      (TArray (IShape [IVar "i"], (TArray (IShape [IVar "j"], TInt))))
  let test_12 _ =
    test_neq
      (TArray (IShape [IVar "i"; IVar "j"], TInt))
      (TArray (IShape [IVar "i"; IVar "j"], TBool))
  let test_13 _ =
    test_eq
      (TArray (IShape [], (TArray (IShape [IVar "i"], TInt))))
      (TArray (IShape [IVar "i"], (TArray (IShape [], TInt))))
  let test_14 _ =
    test_neq (TVar "q") (TVar "w")
  let tests =
    let open OUnit2 in
    "check type equivalence">:::
      ["same base type">:: test_1;
       "different base types">:: test_2;
       "alpha-equiv Pi">:: test_3;
       "Pi with changed shape">:: test_4;
       "Pi with changed type">:: test_5;
       "alpha-equivalent Sigma">:: test_6;
       "Sigma with changed type">:: test_7;
       "Sigma with changed shape">:: test_8;
       "alpha equivalent forall">:: test_9;
       "forall with changed type">:: test_10;
       "nested and non-nested array versions">:: test_11;
       "array with changed type">:: test_12;
       "scalar/vector nesting">:: test_13;
       "different type vars">:: test_14]
end

module Test_frame_contribution : sig
  val tests: U.test
end = struct
  let test_1 _ =
    U.assert_equal
      (frame_contribution (TArray (IShape [], TInt)) (TArray (IShape [], TInt)))
      (Some [IShape []])
  let test_2 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IShape [], TInt)) (TArray (IShape [], TFloat)))
      None
  let test_3 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IShape [], TFloat))
         (TArray (IShape [IVar "l"], TFloat)))
      (Some [IShape [IVar "l"]])
  let test_4 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IShape [], TBool)) (TArray (IVar "d", TBool)))
      (Some [IVar "d"])
  let test_5 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IShape [], TInt))
         (TArray (IShape [IVar "l"; IVar "m"],
                  TArray (IVar "d", TInt))))
      (Some [IShape [IVar "l"]; IShape [IVar "m"]; IVar "d"])
  let test_6 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IShape [INat 3], TInt))
         (TArray (IShape [INat 4], TInt)))
      None
  let test_7 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IShape [IVar "l"], TInt))
         (TArray (IShape [], TInt)))
      None
  let test_8 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IVar "d", TArray (IShape [INat 4], TFloat)))
         (TArray (IVar "d", TArray (IShape [INat 4], TFloat))))
      (Some [IShape []])
  let test_9 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IVar "d1",
                  TArray (IShape [INat 4], TFloat)))
         (TArray (IVar "d2",
                  (TArray (IVar "d1",
                           TArray (IShape [INat 4], TFloat))))))
      (Some [IVar "d2"])
  let test_10 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IVar "d1", TArray (IShape [INat 4], TFloat)))
         (TArray (IVar "d1",
                  TArray (IVar "d2", TArray (IShape [INat 4], TFloat)))))
      None
  let test_11 _ =
    U.assert_equal
      (frame_contribution
         (TArray (IShape [],
                  TDSum (["x", SNat], TArray (IShape [IVar "x"], TFloat))))
         (TArray (IShape [INat 3],
                  TDSum (["y", SNat], TArray (IShape [IVar "y"], TFloat)))))
      (Some [IShape [INat 3]])
  let tests =
    let open OUnit2 in
    "identify the frame portion of this type">:::
      ["scalars">:: test_1;
       "scalars with mismatched element">:: test_2;
       "scalar and vector">:: test_3;
       "scalar and unknown">:: test_4;
       "scalar and matrix of unknown">:: test_5;
       "vectors with mismatched length">:: test_6;
       "vector and scalar">:: test_7;
       "d1 of scalar">:: test_8;
       "d2 of d1 of scalar">:: test_9;
       "d1 of d2 of scalar (wrong nesting order)">:: test_10;
       "scalar and vector of alpha-equivalent element type">:: test_11]
end

let assert_typ_equal (t1: typ) (t2: typ) =
  U.assert_bool "types not equal" (typ_equal t1 t2)

module Test_annot_elt_type : sig
  val tests: U.test
end = struct
  let typ_of_rem_elt ?(idxs=[])
                     ?(typs=[])
                     ?(vars=[])
                     (e: rem_elt) : typ option =
    e |> (annot_elt_init ~init:())
      |> (annot_elt_type idxs typs vars)
      |> annot_of_elt
  let assert_elt_type (e: rem_elt) (check_typ: typ) =
    match (typ_of_rem_elt e) with
    | Some elt_typ -> assert_typ_equal elt_typ check_typ
    | None -> U.assert_failure "expr is ill-typed"
  let assert_ill_typed (e: rem_elt) =
    U.assert_equal (typ_of_rem_elt e) None
  open Test_basic_ast
  let test_6 _ =
    assert_elt_type
      unary_lambda
      (TFun ([TArray (IShape [], TInt)],
             TArray (IShape [], TInt)))
  let test_7 _ =
    assert_elt_type
      binary_lambda
      (TFun ([TArray (IShape [INat 3], TFloat);
              TArray (IShape [INat 1], TBool)],
             TArray (IShape [], TInt)))
  let tests =
    let open OUnit2 in
    "add type annotation to an element node">:::
      ["unary lambda">:: test_6;
       "binary lambda">:: test_7]
end

module Test_annot_expr_type : sig
  val tests: U.test
end = struct
  let typ_of_rem_expr ?(idxs=[])
                      ?(typs=[])
                      ?(vars=[])
                      (e: rem_expr) : typ option =
    e |> (annot_expr_init ~init:())
      |> (annot_expr_type idxs typs vars)
      |> annot_of_expr
  let assert_expr_type (e: rem_expr) (check_typ: typ) =
    match (typ_of_rem_expr e) with
    | Some expr_typ -> assert_typ_equal expr_typ check_typ
    | None -> U.assert_failure "expr is ill-typed"
  let assert_ill_typed (e: rem_expr) =
    U.assert_equal (typ_of_rem_expr e) None
  open Test_basic_ast
  let test_1 _ =
    assert_expr_type
      flat_arr_2_3
      (TArray (IShape [INat 2; INat 3], TInt))
  let test_2 _ = assert_expr_type arr_2 (TArray (IShape [INat 2], TBool))
  let test_3 _ = assert_ill_typed arr_wrong
  let test_4 _ =
    assert_expr_type
      nest_arr_2_3
      (TArray (IShape [INat 2], TArray (IShape [INat 3], TInt)))
  let test_5 _ =
    assert_expr_type
      nest_arr_2_3
      (TArray (IShape [INat 2; INat 3], TInt))
  let test_6 _ =
    assert_expr_type
      unary_app
      (TArray (IShape [], TInt))
  let test_7 _ =
    assert_expr_type
      binary_app
      (TArray (IShape [], TInt))
  let test_8 _ =
    assert_expr_type
      unary_to_nested_app
      (TArray (IShape [INat 2; INat 3], TInt))
  let test_9 _ =
    assert_expr_type
      nested_to_unary_app
      (TArray (IShape [INat 3; INat 2], TInt))
  let test_10 _ =
    assert_expr_type
      dep_sum_create
      (TDSum ([("d", SNat)], TArray (IShape [IVar "d"], TInt)))
  let test_11 _ =
    assert_expr_type
      type_abst
      (TAll (["elt"], TArray (IShape [],
                              TFun ([TArray (IShape [], TVar "elt")],
                                    TArray (IShape [], TVar "elt")))))
  let test_12 _ = assert_ill_typed type_abst_bad
  let test_13 _ =
    assert_expr_type
      type_app
      (TArray (IShape [],
               TFun ([TArray (IShape [], TBool)],
                     TArray (IShape [], TBool))))
  let test_14 _ =
    assert_expr_type
      index_abst
      (TDProd (["d", SNat],
               TArray (IShape [], TFun ([TArray (IShape [IVar "d"], TInt)],
                                        TArray (IShape [IVar "d"], TInt)))))
  let test_15 _ =
    assert_expr_type
      index_app
      (TArray (IShape [],
               (TFun ([TArray (IShape [INat 6], TInt)],
                      TArray (IShape [INat 6], TInt)))))
  let test_16 _ = assert_expr_type dep_sum_project (TArray (IShape [], TInt))
  let test_17 _ =
    assert_expr_type
      remora_compose
      (TDProd
         (["s1", SShape; "s2", SShape; "s3", SShape],
          TAll
            (["alpha"; "beta"; "gamma"],
             TArray (IShape [],
                     TFun ([TArray (IShape [],
                                    TFun ([TArray (IVar "s1", TVar "alpha")],
                                          TArray (IVar "s2", TVar "beta")));
                            TArray (IShape [],
                                    TFun ([TArray (IVar "s2", TVar "beta")],
                                          TArray (IVar "s3", TVar "gamma")))],
                           TArray (IShape [],
                                   TFun ([TArray (IVar "s1", TVar "alpha")],
                                         TArray (IVar "s3", TVar "gamma"))))))))
  let tests =
    let open OUnit2 in
    "add type annotation to an expression node">:::
      ["2x3 of int">:: test_1;
       "2 of boolean">:: test_2;
       "mismatching item shapes">:: test_3;
       "2 of 3 of int">:: test_4;
       "2 of 3 of int as 2x3 of int">:: test_5;
       "unary application">:: test_6;
       "binary application">:: test_7;
       "apply unary function to nested arg">:: test_8;
       "apply nested function to vector arg">:: test_9;
       "construct dependent sum">:: test_10;
       "type abstraction">:: test_11;
       "malformed type abstraction">:: test_12;
       "type application">:: test_13;
       "index abstraction">:: test_14;
       "index application">:: test_15;
       "destruct dependent sum">:: test_16;
       "function composition">:: test_17]
end

module UnitTests : sig
  val tests : U.test
end = struct
  let tests =
    let open OUnit2 in
    "typechecker tests">:::
      [Test_env_update.tests;
       Test_srt_of_idx.tests;
       Test_kind_of_typ.tests;
       Test_uniq_typ.tests;
       Test_shape_of_typ.tests;
       Test_elt_of_typ.tests;
       Test_canonicalize_typ.tests;
       Test_typ_equal.tests;
       Test_frame_contribution.tests;
       Test_annot_elt_type.tests;
       Test_annot_expr_type.tests]
end
