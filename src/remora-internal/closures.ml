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
module MR = Map_replicate_ast;;
module B = Basic_ast;;
open Frame_notes

type var = Basic_ast.var with sexp

type 'a cl_app_t = {closure: 'a; args: 'a list;} with sexp
type 'a closure_t = {code: 'a; env: 'a;} with sexp

type 'a expr_form =
| App of 'a cl_app_t
| Vec of 'a MR.vec_t
| Map of 'a MR.map_t
| Rep of 'a MR.rep_t
| Tup of 'a MR.tup_t
| Let of 'a MR.let_t
| Cls of 'a closure_t
| Lam of 'a MR.lam_t
| Var of var
| Int of int
| Float of float
| Bool of bool
with sexp

let map_expr_form
    ~(f: 'a -> 'b)
    (e: 'a expr_form) : 'b expr_form =
  match e with
  | App {closure = c; args = a} -> App {closure = f c;
                                        args = List.map ~f:f a}
  | Vec {MR.dims = d; MR.elts = e} -> Vec {MR.dims = d;
                                           MR.elts = List.map ~f:f e}
  | Map {MR.frame = fr; MR.fn = fn; MR.args = a; MR.shp = shp} ->
    Map {MR.frame = f fr;
         MR.fn = f fn;
         MR.args = List.map ~f:f a;
         MR.shp = f shp}
  | Rep {MR.arg = a; MR.new_frame = n; MR.old_frame = o} ->
    Rep {MR.arg = f a; MR.new_frame = f n; MR.old_frame = f o}
  | Tup e -> Tup (List.map ~f:f e)
  | Let {MR.vars = v; MR.bound = bn; MR.body = bd} ->
    Let {MR.vars = v; MR.bound = f bn; MR.body = f bd}
  | Cls {code = c; env = a} -> Cls {code = f c; env = f a}
  | Lam {MR.bindings = v; MR.body = e} ->
    Lam {MR.bindings = v; MR.body = f e}
  | Var _ | Int _ | Float _ | Bool _ as v -> v

type expr = Expr of expr expr_form with sexp
type defn = Defn of var * expr with sexp
type prog = Prog of defn list * expr with sexp

type 'annot ann_expr = AExpr of 'annot * ('annot ann_expr) expr_form with sexp
type 'annot ann_defn = ADefn of var * 'annot ann_expr with sexp
type 'annot ann_prog =
  AProg of 'annot * 'annot ann_defn list * 'annot ann_expr with sexp

let rec expr_of_maprep
    (bound_vars: var list)
    (MR.AExpr (a, e): 'a MR.ann_expr)
    : 'a ann_expr =
  AExpr (a,
         match e with
         | MR.App {MR.fn = f; MR.args = a} ->
           App {closure = expr_of_maprep bound_vars f;
                args = List.map ~f:(expr_of_maprep bound_vars) a}
         | MR.Vec {MR.dims = d; MR.elts = e} ->
           Vec {MR.dims = d;
                MR.elts = List.map ~f:(expr_of_maprep bound_vars) e}
         | MR.Map {MR.frame = fr; MR.fn = fn; MR.args = a; MR.shp = s} ->
           Map {MR.frame = expr_of_maprep bound_vars fr;
                MR.fn = expr_of_maprep bound_vars fn;
                MR.args = List.map ~f:(expr_of_maprep bound_vars) a;
                MR.shp = expr_of_maprep bound_vars s}
         | MR.Rep {MR.arg = a; MR.new_frame = n; MR.old_frame = o} ->
           Rep {MR.arg = expr_of_maprep bound_vars a;
                MR.new_frame = expr_of_maprep bound_vars n;
                MR.old_frame = expr_of_maprep bound_vars o}
         | MR.Tup e -> Tup (List.map ~f:(expr_of_maprep bound_vars) e)
         | MR.Let {MR.vars = v; MR.bound = bn; MR.body = bd} ->
           Let {MR.vars = v;
                MR.bound = expr_of_maprep bound_vars bn;
                MR.body = expr_of_maprep bound_vars bd}
         | MR.Lam {MR.bindings = v; MR.body = b} ->
           let env_name = Basic_ast.gensym "__ENV_"
           (* Exclude variables bound by this lambda from the resulting free
              variable list. *)
           and bound_for_body = List.append v bound_vars in
           let free_vars = MR.aexpr_free_vars bound_for_body b in
           Cls {code = AExpr
               (a,
                Lam {MR.bindings = env_name :: v;
                     MR.body = AExpr
                    (a, Let {MR.vars = free_vars;
                             MR.bound = AExpr (a, Var env_name);
                             MR.body = expr_of_maprep bound_for_body b})});
                env = AExpr (a, Tup (List.map ~f:(fun v -> AExpr (a, Var v))
                                       free_vars))}
         | MR.Var v -> Var v
         | MR.Int i -> Int i
         | MR.Float f -> Float f
         | MR.Bool b -> Bool b)
let defn_of_maprep
    (bound_vars: var list)
    (MR.ADefn (name, body): 'a MR.ann_defn)
    : 'a ann_defn =
  (* We include the defn-bound name just in case it wasn't passed in. *)
  ADefn (name, expr_of_maprep (name :: bound_vars) body)
(* Can optionally pass in a list of built-in names *)
let prog_of_maprep
    ?(bound_vars = [])
    (MR.AProg (a, defns, expr): 'a MR.ann_prog)
    : 'a ann_prog =
  let top_level_names = List.map ~f:(fun (MR.ADefn (n, _)) -> n) defns in
  AProg (a, List.map ~f:(defn_of_maprep top_level_names) defns,
         expr_of_maprep top_level_names expr)

let rec annot_expr_drop (AExpr (_, e): 'a ann_expr) : expr =
  Expr (map_expr_form ~f:annot_expr_drop e)
let annot_defn_drop (ADefn (name, body): 'a ann_defn) : defn =
  Defn (name, annot_expr_drop body)
let annot_prog_drop (AProg (_, defns, expr): 'a ann_prog) : prog =
  Prog (List.map ~f:annot_defn_drop defns, annot_expr_drop expr)

(* A computed value of type 'v, along with a list of definitions accumulated
   in the process of computing it, and a monadic interface for working with
   these structures.
   TODO: May want to use something other than a list for accumulating
   definitions if appending gets too slow. *)
module Defn_writer : sig
  type ('v, 'a) t = 'v * 'a ann_defn list
  val (>>=) : ('v, 'a) t -> ('v -> ('w, 'a) t) -> ('w, 'a) t
  val (>>|) : ('v, 'a) t -> ('v -> 'w) -> ('w, 'a) t
  val (>>) : ('v, 'a) t -> ('w, 'a) t -> ('w, 'a) t
  val bind : ('v, 'a) t -> ('v -> ('w, 'a) t) -> ('w, 'a) t
  val return : 'v -> ('v, 'a) t
  val map : ('v, 'a) t -> f:('v -> 'w) -> ('w, 'a) t
  val join : (('v, 'a) t, 'a) t -> ('v, 'a) t
  val all : ('v, 'a) t list -> ('v list, 'a) t
  val tell : 'a ann_defn list -> (unit, 'a) t
end = struct
  type ('v, 'a) t = 'v * 'a ann_defn list
  let (>>=)
      ((val_in, defns_in): ('v, 'a) t)
      (f: 'v -> ('w, 'a) t) : ('w, 'a) t =
    let (val_ret, defns_ret) = f val_in in
    (val_ret, List.append defns_in defns_ret)
  let (>>|)
      ((val_in, defns_in): ('v, 'a) t)
      (f: 'v -> 'w) : ('w, 'a) t =
    (f val_in, defns_in)
  let (>>) x y = x >>= (fun _ -> y)
  let bind v f = v >>= f
  let return v = (v, [])
  let map t ~f = t >>| f
  let join t = t >>= (fun t' -> t')
  let all ts = (List.map ~f:fst ts, List.join (List.map ~f:snd ts))
  let tell new_defns = ((), new_defns)
end

(* Traverse an expression, replacing Lam forms with fresh Var forms and
   generating a list of (annotated) definitions for those variables. *)
let rec expr_hoist_lambdas
    ((AExpr (a, e): 'a ann_expr) as expr)
    : ('a ann_expr, 'a) Defn_writer.t =
  let open Defn_writer in
  (* In almost all cases, we just recur on all subexpressions and merge
     their results together. *)
  match e with
  | App {closure = clos; args = args} ->
    expr_hoist_lambdas clos >>= fun new_clos ->
    List.map ~f:expr_hoist_lambdas args |> all >>= fun new_args ->
    AExpr (a, App {closure = new_clos; args = new_args}) |> return
  | Vec {MR.dims = dims; MR.elts = elts} ->
    List.map ~f:expr_hoist_lambdas elts |> all >>= fun new_elts ->
    AExpr (a, Vec {MR.dims = dims; MR.elts = new_elts}) |> return
  | Map {MR.frame = frame; MR.fn = fn; MR.args = args; MR.shp = shp} ->
    expr_hoist_lambdas frame >>= fun new_frame ->
    expr_hoist_lambdas fn >>= fun new_fn ->
    List.map ~f:expr_hoist_lambdas args |> all >>= fun new_args ->
    expr_hoist_lambdas shp >>= fun new_shp ->
    AExpr (a, Map {MR.frame = new_frame;
                   MR.fn = new_fn;
                   MR.args = new_args;
                   MR.shp = new_shp}) |> return
  | Rep {MR.arg = arg; MR.old_frame = oldf; MR.new_frame = newf} ->
    expr_hoist_lambdas arg >>= fun new_arg ->
    expr_hoist_lambdas oldf >>= fun new_oldf ->
    expr_hoist_lambdas newf >>= fun new_newf ->
    AExpr (a, Rep {MR.arg = new_arg;
                   MR.old_frame = new_oldf;
                   MR.new_frame = new_newf}) |> return
  | Tup elts ->
    List.map ~f:expr_hoist_lambdas elts |> all >>= fun new_elts ->
    AExpr (a, Tup new_elts) |> return
  | Let {MR.vars = vars; MR.bound = bound; MR.body = body} ->
    expr_hoist_lambdas bound >>= fun new_bound ->
    expr_hoist_lambdas body >>= fun new_body ->
    AExpr (a, Let {MR.vars = vars;
                   MR.bound = new_bound;
                   MR.body = new_body}) |> return
  | Cls {code = code; env = env} ->
    expr_hoist_lambdas code >>= fun new_code ->
    expr_hoist_lambdas env >>= fun new_env ->
    AExpr (a, Cls {code = new_code; env = new_env}) |> return
  (* This is the only interesting case. *)
  | Lam {MR.bindings = vars; MR.body = body} ->
    (* Generate a new name to use as a global variable. *)
    let new_global = B.gensym "__HOIST_" in
    (* Hoist any lambdas that appear within the function body. *)
    expr_hoist_lambdas body >>= fun new_body ->
    (* Emit a new definition for this function, with the converted body. *)
    tell [ADefn (new_global, AExpr (a, Lam {MR.bindings = vars;
                                            MR.body = new_body}))] >>
    (* Replace this function with the global variable. *)
    (AExpr (a, Var new_global) |> return)
  | Var _ | Int _ | Float _ | Bool _ -> return expr

module Passes : sig
  val prog : 'a MR.ann_prog -> 'a ann_prog
  val defn : 'a MR.ann_defn -> 'a ann_defn
  val expr : 'a MR.ann_expr -> 'a ann_expr

  val prog_all : B.rem_prog -> (arg_frame * app_frame) ann_prog option
  val defn_all : B.rem_defn -> (arg_frame * app_frame) ann_defn option
  val expr_all : B.rem_expr -> (arg_frame * app_frame) ann_expr option
  val elt_all : B.rem_elt -> (arg_frame * app_frame) ann_expr option
end = struct
  let lib_vars = []
  let prog remora = remora |> prog_of_maprep ~bound_vars:lib_vars
  let defn remora = remora |> defn_of_maprep lib_vars
  let expr remora = remora |> expr_of_maprep lib_vars
  open Option.Monad_infix
  let prog_all remora = remora |> MR.Passes.prog_all >>| prog
  let defn_all remora = remora |> MR.Passes.defn_all >>| defn
  let expr_all remora = remora |> MR.Passes.expr_all >>| expr
  let elt_all remora = remora |> MR.Passes.elt_all >>| expr
end
