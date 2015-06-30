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
open Core.Std
open Core.Option
open Option.Monad_infix
open Map_replicate_ast
module T = Typechecker;;
module U = OUnit2;;

let rec subst_expr_form
    (recur: 'value T.env -> 'subexp -> 'subexp)
    (subst: 'value T.env)
    (ef: 'subexp expr_form) : 'subexp expr_form =
  match ef with
  | Var name -> List.Assoc.find subst name |> Option.value ~default:(Var name)
  | Lam {bindings = bindings; body = body} ->
    Lam {bindings = bindings;
         body = (recur
                   (List.fold ~init:subst
                      ~f:(List.Assoc.remove ~equal:(=)) bindings) body)}
  | Let {vars = vars; bound = bound; body = body} ->
    Let {vars = vars;
         bound = (recur subst bound);
         body = (recur (List.fold ~init:subst
                          ~f:(List.Assoc.remove ~equal:(=)) vars) body)}
  | _ -> map_expr_form ~f:(recur subst) ef

let rec split_list (increment: int) (xs: 'a list) : 'a list list =
  match List.take xs increment with
  | [] -> []
  | first_block ->
    first_block :: split_list increment (List.drop xs increment)

let rec repeat_list (count: int) (xs: 'a list) : 'a list =
  if count <= 0
  then []
  else List.append xs (repeat_list (count - 1) xs)

let known_shape (Expr e) : int list =
  match e with
  | Vec {dims = dims; elts = _} -> dims
  | _ -> []

let is_Int (Expr e) = (match e with | Int _ -> true | _ -> false)

let to_int (Expr e) = (match e with | Int i -> i | _ -> assert false)

(* TODO: for scalar frame, arg cells should not get array-wrapped *)
let rec cell_split ~(frame: int list) ((Expr array): expr) : expr list option =
  match array with
  | Vec {dims = dims; elts = elts} ->
    let cell_shape = List.drop dims (List.length frame) in
    let cell_size = List.fold_right ~f:( * ) ~init:1 cell_shape in
    let cell_elts = split_list cell_size elts in
    if cell_shape = []
    then List.map ~f:List.hd cell_elts |> Option.all
    else Some (List.map
                 ~f:(fun c -> Expr (Vec {dims = cell_shape; elts = c}))
                 cell_elts)
  | _ -> None

let apply_primop (opname: var) (args: expr list) : expr =
  match opname with
  | "append" ->
    (* Make sure all args agree on shape past the 1st axis. *)
    (try (let c::cs = List.map
            ~f:(fun (Expr (Vec {dims = _::cell; elts = _})) -> cell) args in
          let new_1st_axis = List.fold_right ~init:0
            ~f:(fun (Expr (Vec {dims = d::_; elts = _})) accum -> d + accum)
            args in
          if (List.for_all ~f:(fun z -> c = z) cs)
          then Expr
            (Vec {dims = new_1st_axis::c;
                  elts = List.join
                (List.map ~f:(fun (Expr (Vec {dims = _; elts = e})) -> e)
                   args)})
          else Expr (App {fn = Expr (Var opname); args = args})
     (* Some arg wasn't reduced to a Vec form. *)
     ) with _ -> Expr (App {fn = Expr (Var opname); args = args}))
  | _ -> Expr (App {fn = Expr (Var opname); args = args})

(* Expression evaluator, to allow more flexible tests. *)
let rec eval_expr ~(env: expr T.env) (Expr e) : expr =
    match e with
    | App {fn = fn; args = args} ->
      let fn_val = eval_expr ~env:env fn
      and args_val = List.map ~f:(eval_expr ~env:env) args in
      (match fn_val with
      | Expr (Lam {bindings = bindings; body = body}) ->
        eval_expr ~env:(List.append (List.zip_exn bindings args_val) env) body
      (* Special recognition for a few primitive ops *)
      | Expr (Var op) ->
        eval_expr ~env:env (apply_primop op args_val)
      (* Unrecognized/un-evaluated operator *)
      | _ -> Expr (App {fn = fn_val; args = args_val}))
    | Vec {dims = dims; elts = elts} ->
      let elts_val = List.map ~f:(eval_expr ~env:env) elts in
      (* If the evaluated elts all match in shape, collapse one nest level. *)
        (match elts_val with
        | (Expr (Vec el)) :: els ->
          if (List.for_all ~f:(fun (Expr (Vec i)) -> i.dims = el.dims) els)
          then let (joined_dims, joined_elts) =
                 (List.append dims el.dims,
                  List.join (List.map ~f:(fun (Expr (Vec i)) -> i.elts)
                               elts_val))
               in eval_expr ~env:env (Expr (Vec {dims = joined_dims;
                                                 elts = joined_elts}))
          else Expr (Vec {dims = dims; elts = elts_val})
        | _ -> Expr (Vec {dims = dims; elts = elts_val})) (* in *)
        (* Expr (Vec {dims = joined_dims; elts = joined_elts}) *)
    | Map {frame = frame; fn = fn; args = args; shp = shp} ->
      let frame_val = eval_expr ~env:env frame
      and fn_val = eval_expr ~env:env fn
      and args_val = List.map ~f:(eval_expr ~env:env) args
      and shp_val = eval_expr ~env:env shp in
      let eval_stuck = Expr (Map {frame = frame_val;
                                  fn = fn_val;
                                  args = args_val;
                                  shp = shp_val}) in
      let args_axes = List.map ~f:known_shape args_val in
      (match (fn_val, frame_val, shp_val) with
      (* We need fn_val to be a Lam and frame and shp to be valid shapes. *)
      | (Expr (Lam {bindings = bindings; body = body}),
         Expr (Vec {dims = [rank]; elts = frame_elts}),
         Expr (Vec {dims = [result_rank]; elts = shp_elts})) ->
        if List.for_all ~f:is_Int frame_elts
        then
          let frame_axes = List.map ~f:to_int frame_elts in
          (* We need args to be arrays with enough leading axes for mapping.*)
          if (List.for_all
                ~f:(fun s -> T.prefix_of frame_axes s = Some true)
                args_axes)
          then
            ((Option.all
                (List.map  ~f:(cell_split ~frame:frame_axes) args_val)
              >>= fun args_cells ->
              List.transpose args_cells >>= fun transp_cells ->
              let apps = List.map
                ~f:(fun cells -> Expr (App {fn = fn_val; args = cells}))
                transp_cells in
              return
                (if List.length transp_cells = 0
                 (* No result cells, so use the declared result shape. *)
                 then eval_expr ~env:env
                    (Expr (Vec {dims = List.map ~f:to_int shp_elts; elts = []}))
                 (* Evaluate the vector of result cells. *)
                 else eval_expr ~env:env
                    (Expr (Vec {dims = frame_axes; elts = apps}))))
                |> Option.value ~default:eval_stuck)
          else eval_stuck
        else eval_stuck
      | _ -> eval_stuck)
    | Rep {arg = arg; old_frame = old_frame; new_frame = new_frame} ->
      let arg_val = eval_expr ~env:env arg
      and old_frame_val = eval_expr ~env:env old_frame
      and new_frame_val = eval_expr ~env:env new_frame in
      let eval_stuck = Expr (Rep {arg = arg_val;
                                  old_frame = old_frame_val;
                                  new_frame = new_frame_val}) in
      (* Make sure we have evaluated everything far enough that we know the
         entire old_frame and new_frame. *)
      (match (arg_val, old_frame_val, new_frame_val) with
      | (Expr (Vec {dims = arg_val_dims; elts = arg_val_elts}),
         Expr (Vec {dims = [old_frame_rank]; elts = old_frame_axes}),
         Expr (Vec {dims = [new_frame_rank]; elts = new_frame_axes})) ->
        (* Make sure that the old_frame is a prefix of the new_frame, and
           that the old_frame is a prefix of arg's shape. *)
        if (List.for_all ~f:is_Int old_frame_axes)
          && (List.for_all ~f:is_Int new_frame_axes)
          && (List.length old_frame_axes = old_frame_rank)
          && (List.length new_frame_axes = new_frame_rank)
          && (T.prefix_of old_frame_axes new_frame_axes = Some true)
          && (T.prefix_of (List.map ~f:to_int old_frame_axes) arg_val_dims
              = Some true)
        (* Copy each cell of the argument. *)
        (* 1. identify (visible portion of) cell shape *)
        (* 2. split arg_val_elts into cells *)
        then
          let expansion_size = List.fold_right ~init:1 ~f:( * )
            (List.map ~f:to_int (List.drop new_frame_axes old_frame_rank))
          and cell_shape = (List.drop arg_val_dims old_frame_rank) in
          let cell_size = List.fold_right ~init:1 ~f:( * ) cell_shape in
          let cells = split_list cell_size arg_val_elts in
          let more_cells: expr list =
            List.join (List.map ~f:(repeat_list expansion_size) cells) in
          let more_dims =
            List.append (List.map ~f:to_int new_frame_axes) cell_shape in
          Expr (Vec {dims = more_dims; elts = more_cells})
        else eval_stuck
      | _ -> eval_stuck
      )
    | Tup elts -> Expr( Tup (List.map ~f:(eval_expr ~env:env) elts))
    | Lam {bindings = bindings; body = body} as lam -> Expr lam
    | Let {vars = vars; bound = bound; body = body} ->
      let bound_val = eval_expr ~env:env bound in
      (match bound_val with
      | Expr Tup elts ->
        eval_expr ~env:(List.append (List.zip_exn vars elts) env) body
      | _ -> Expr (Let {vars = vars; bound = bound_val; body = body}))
    | Var name ->
      List.Assoc.find env name |> Option.value ~default:(Expr (Var name))
    | Bool _ | Float _ | Int _ as c -> Expr c
;;

let flat_arr_2_3 =
  Expr (Vec {dims = [2; 3]; elts = [Expr (Int 4); Expr (Int 1); Expr (Int 6);
                                    Expr (Int 2); Expr (Int 3); Expr (Int 5)]})
let arr_2 = Expr (Vec {dims = [2];
                       elts = [Expr (Bool false); Expr (Bool true)]})
let unary_lambda = Expr (Lam {bindings = ["x"];
                              body = Expr (Vec {dims = [];
                                               elts = [Expr (Int 3)]})})
let binary_lambda = Expr (Lam {bindings = ["x"; "y"];
                               body = Expr (Vec {dims = [];
                                                 elts = [Expr (Int 3)]})})
let unary_app = Expr (Vec {dims = []; elts = [Expr (Int 3)]})
let unary_to_nested_app =
  Expr (Vec {dims = [2; 3]; elts = [Expr (Int 1); Expr (Int 2); Expr (Int 3);
                                    Expr (Int 4); Expr (Int 5); Expr (Int 6)]})
let nested_to_unary_app =
  Expr (Vec {dims = [3; 2]; elts = [Expr (Int 1); Expr (Int 2); Expr (Int 3);
                                    Expr (Int 4); Expr (Int 5); Expr (Int 6)]})
let type_abst = Expr (Vec {dims = [];
                           elts = [Expr (Lam {bindings = ["x"];
                                              body = Expr (Var "x")})]})
let index_abst =
  Expr (Lam {bindings = ["__I_d"];
             body = Expr (Vec {dims = [];
                               elts = [Expr (Lam {bindings = ["l"];
                                                  body = Expr (Var "l")})]})})
let index_app =
Expr (Vec {dims = [];
           elts = [Expr (Lam {bindings = ["l"];
                              body = Expr (Var "l")})]})
let dep_sum_create =
  Expr (Tup [Expr (Vec {dims = [3];
                        elts = [Expr (Int 0); Expr (Int 1); Expr (Int 2)]});
             Expr (Int 3)])
let dep_sum_project =
  Expr (Vec {dims = []; elts = [Expr (Int 0)]})

module Test_translation : sig
  val tests : U.test
end = struct
  let assert_translate_expr original final =
    U.assert_equal
      (original
          |> Passes.expr_all
          |> Option.map ~f:annot_expr_drop
          |> Option.map ~f:(eval_expr ~env:[]))
      (Some final)
  let assert_translate_elt original final =
    U.assert_equal
      (original
          |> Passes.elt_all
          |> Option.map ~f:annot_expr_drop
          |> Option.map ~f:(eval_expr ~env:[]))
      (Some final)
  module TB = Test_basic_ast;;
  let test_1 _ =
    assert_translate_expr TB.flat_arr_2_3 flat_arr_2_3
  let test_2 _ =
    assert_translate_expr TB.arr_2 arr_2
  let test_3 _ =
    assert_translate_expr TB.nest_arr_2_3 flat_arr_2_3
  let test_4 _ =
    assert_translate_elt TB.unary_lambda unary_lambda
  let test_5 _ =
    assert_translate_elt TB.binary_lambda binary_lambda
  let test_6 _ =
    assert_translate_expr TB.unary_app unary_app
  let test_7 _ =
    assert_translate_expr TB.binary_app unary_app
  let test_8 _ =
    assert_translate_expr TB.unary_to_nested_app unary_to_nested_app
  let test_9 _ =
    assert_translate_expr TB.nested_to_unary_app nested_to_unary_app
  let test_10 _ =
    assert_translate_expr TB.type_abst type_abst
  let test_11 _ =
    assert_translate_expr TB.type_app type_abst
  let test_12 _ =
    assert_translate_expr TB.index_abst index_abst
  let test_13 _ =
    assert_translate_expr TB.index_app index_app
  let test_14 _ =
    assert_translate_expr TB.dep_sum_create dep_sum_create
  let test_15 _ =
    assert_translate_expr TB.dep_sum_project dep_sum_project
  let tests =
    let open OUnit2 in
    "translate from basic AST to Map/Replicate & evaluate">:::
      ["flat 2x3">:: test_1;
       "2-vector">:: test_2;
       "collapse nested 2x3 to flat">:: test_3;
       "unary lambda">:: test_4;
       "binary lambda">:: test_5;
       "unary app producing 3-vector">:: test_6;
       "binary app producing 3-vector">:: test_7;
       "unary-to-nested produces flat 2x3">:: test_8;
       "nested-to-unary produces flat 3x2">:: test_9;
       "type abstraction erased">:: test_10;
       "type application erased">:: test_11;
       "index abstraction becomes term abstraction">:: test_12;
       "index application becomes term application">:: test_13;
       "dependent sum becomes tuple">:: test_14;
       "destruct tuple by let-binding">:: test_15]
end

module UnitTests : sig
  val tests : U.test
end = struct
  let tests =
    let open OUnit2 in
    "Map-Replicate AST tests">:::
      [Test_translation.tests]
end
