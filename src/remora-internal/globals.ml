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

let scalar t = TArray (IShape [], t)
let vec l t = TArray (IShape [l], t)
let vecv l t = vec (IVar l) t
let vecn l t = vec (INat l) t
let func ins out = scalar (TFun (ins, out))

let arith_unary t =
  func [scalar t] (scalar t)
let arith_binary t =
  func [scalar t; scalar t] (scalar t)
let compare t =
  func [scalar t; scalar t] (scalar TBool)

let any i t = TArray (IVar i, TVar t)

let assoc_all names typ = List.map ~f:(fun n -> (n,typ)) names

let int_arith_binary =
  assoc_all ["+"; "-"; "*"; "/"; "^"; "rand"]
    (arith_binary TInt)

let float_arith_binary =
  assoc_all ["+."; "-."; "*."; "/."; "^.";]
    (arith_binary TFloat)
let float_arith_unary =
  assoc_all ["sin"; "cos"; "tan"; "log"; "lg"; "ln"; "sqrt"; "e^"]
    (arith_unary TFloat)

let logic_binary =
  assoc_all ["and"; "or"; "xor"]
    (arith_binary TBool)
let logic_unary =
  assoc_all ["not"]
    (arith_unary TBool)

let int_compare =
  assoc_all [">"; ">="; "<"; "<="; "="; "!="]
    (compare TInt)

let float_compare =
  assoc_all [">."; ">=."; "<."; "<=."; "=."; "!=."]
    (compare TFloat)

let num_coerce =
  ("float", func [TInt] TFloat) ::
    (assoc_all ["round"; "floor"; "ceil"] (func [TFloat] TInt))

let choice =
  let any = any "s" "t" in
  ["choice",
    TDProd (["s", SShape], TAll (["t"], func [scalar TBool; any; any] any))]

let head_tail =
  let any = any "s" "t" in
  assoc_all ["head"; "tail"]
    (TDProd (["l", SNat; "s", SShape],
             TAll (["t"],
                   func [TArray (IShape [ISum (IVar "l", INat 1)], any)] any)))

let behead_curtail =
  let any = any "s" "t" in
  assoc_all ["head"; "tail"]
    (TDProd (["l", SNat; "s", SShape],
             TAll (["t"],
                   func [TArray (IShape [ISum (IVar "l", INat 1)], any)]
                     (TArray (IShape [IVar "l"], any)))))

let take_drop =
  let any = any "s" "t" in
  assoc_all ["take"; "take-right"; "drop"; "drop-right"]
    (TDProd (["l", SNat; "s", SShape],
             TAll (["t"], func [scalar TInt; vecv "l" any]
               (TDSum (["n", SNat], vecv "n" any)))))

let take_witness =
  let any = any "s" "t" in
  assoc_all ["take*"; "take-right*"]
    (TDProd (["l", SNat; "m", SNat; "s", SShape],
             TAll (["u"; "t"], func [vecv "l" (TVar "u");
                                     vec (ISum (IVar "l", IVar "m")) any]
               (vecv "l" any))))

let drop_witness =
  let any = any "s" "t" in
  assoc_all ["drop*"; "drop-right*"]
    (TDProd (["l", SNat; "m", SNat; "s", SShape],
             TAll (["u"; "t"], func [vecv "l" (TVar "u");
                                     vec (ISum (IVar "l", IVar "m")) any]
               (vecv "m" any))))

let reverse =
  let any = any "s" "t" in
  ["reverse", TDProd (["s", SShape],
                      TAll (["t"],
                            func [any] any))]

let rotate =
  let any = any "s" "t" in
  ["rotate", TDProd (["l", SNat; "s", SShape],
                     TAll (["t"],
                           func [scalar TInt; vecv "l" any] (vecv "l" any)))]

let append =
  let any = any "s" "t" in
  ["append", TDProd (["l", SNat; "m", SNat; "s", SShape],
                     TAll (["t"],
                           func [vecv "l" any; vecv "m" any]
                             (vec (ISum (IVar "l", IVar "m")) any)))]

let itemize =
  let any = any "s" "t" in
  ["itemize", TDProd (["s", SShape],
                      TAll (["t"],
                            func [any]
                              (vecn 1 any)))]

let ravel_shape =
  assoc_all ["ravel"; "shape"]
    (TDProd (["s", SShape],
             TAll (["t"],
                   func [any "s" "t"]
                     (TDSum ([], vecv "l" (TVar "t"))))))

let length =
  ["length", TDProd (["s", SShape],
                     TAll (["t"],
                           func [any "s" "t"] (scalar TInt)))]

let left_fold =
  let anyl = any "sl" "tl"
  and anyr = any "sr" "tr" in
  ["foldl", TDProd (["l", SNat; "sl", SShape; "sr", SShape],
                    TAll (["tl"; "tr"],
                          func [func [anyr; anyl] anyl;
                                anyl;
                                vecv "l" anyr]
                            anyl))]

let right_fold =
  let anyl = any "sl" "tl"
  and anyr = any "sr" "tr" in
  ["foldr", TDProd (["l", SNat; "sl", SShape; "sr", SShape],
                    TAll (["tl"; "tr"],
                          func [func [anyr; anyl] anyr;
                                anyr;
                                vecv "l" anyl]
                            anyr))]

let left_scan =
  let anyl = any "sl" "tl"
  and anyr = any "sr" "tr" in
  ["scanl", TDProd (["l", SNat; "sl", SShape; "sr", SShape],
                    TAll (["tl"; "tr"],
                          func [func [anyr; anyl] anyl;
                                anyl;
                                vecv "l" anyr]
                            (vecv "l" anyl)))]

let right_scan =
  let anyl = any "sl" "tl"
  and anyr = any "sr" "tr" in
  ["scanr", TDProd (["l", SNat; "sl", SShape; "sr", SShape],
                    TAll (["tl"; "tr"],
                          func [func [anyr; anyl] anyr;
                                anyr;
                                vecv "l" anyl]
                            (vecv "l" anyr)))]

let reduce =
  let any = any "s" "t" in
  ["reduce", TDProd (["l", SNat; "s", SShape],
                     TAll (["t"],
                           func [func [any; any] any;
                                 vecv "l" any]
                             any))]

let filter =
  let any = any "s" "t" in
  ["filter", TDProd (["l", SNat; "s", SShape],
                     TAll (["t"],
                           func [vecv "l" (scalar TBool);
                                 vecv "l" any]
                             (TDSum (["m", SNat], vecv "m" any))))]

let iota =
  ["iota", TDProd (["l", SNat],
                   func [vecv "l" TInt]
                     (TDSum (["s", SShape], TArray (IVar "s", TInt))))]

let iota_vector =
  ["iotavec", func [scalar TInt] (TDSum (["l", SNat], vecv "l" TInt))]

let iota_witness =
  ["iota*", TDProd (["s", SShape],
                    TAll (["t"],
                          func [any "s" "t"] (TArray (IVar "s", TInt))))]

let read name t =
  [name, func [] (TDSum (["s", SShape], TArray (IVar "s", t)))]
let readvec name t =
  [name, func [] (TDSum (["l", SNat], vecv "l" t))]
let write name t =
  [name, TDProd (["s", SShape],
                 func [TArray (IVar "s", t)] TBool)]

let read_basetype =
  List.join [read "read_i" TInt;
             read "read_f" TFloat;
             read "read_b" TBool]
let readvec_basetype =
  List.join [readvec "readvec_i" TInt;
             readvec "readvec_f" TFloat;
             readvec "readvec_b" TBool]
let write_basetype =
  List.join [write "write_i" TInt;
             write "write_f" TFloat;
             write "write_b" TBool]

let builtins =
  List.join [int_arith_binary;
             float_arith_binary;
             float_arith_unary;
             logic_binary;
             logic_unary;
             int_compare;
             float_compare;
             num_coerce;
             choice;
             head_tail;
             behead_curtail;
             take_drop;
             take_witness;
             drop_witness;
             reverse;
             rotate;
             append;
             itemize;
             ravel_shape;
             length;
             left_fold;
             right_fold;
             left_scan;
             right_scan;
             reduce;
             filter;
             iota;
             iota_vector;
             iota_witness;
             read_basetype;
             readvec_basetype;
             write_basetype]
