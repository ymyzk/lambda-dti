open Format

open OUnit2

open Lambda_rti
open Syntax
open Pp

let id x = x

let parse str =
  Parser.toplevel Lexer.main @@ Lexing.from_string str

let test_pp_ty =
  let test (expected, u) =
    expected >:: fun ctxt ->
      assert_equal ~ctxt:ctxt ~printer:id expected @@ asprintf "%a" pp_ty u
  in
  List.map test [
    "int -> bool", TyFun (TyInt, TyBool);
    "int -> bool -> ?", TyFun (TyInt, TyFun (TyBool, TyDyn));
    "(int -> bool) -> ?", TyFun (TyFun (TyInt, TyBool), TyDyn);
    "(int -> bool) -> ? -> int", TyFun (TyFun (TyInt, TyBool), TyFun (TyDyn, TyInt));
    "'x1 -> 'x2", TyFun (TyVar 1, TyVar 2);
  ]

let test_pp_ty2 =
  let test (expected, u) =
    expected >:: fun ctxt ->
      assert_equal ~ctxt:ctxt ~printer:id expected @@ asprintf "%a" pp_ty2 u
  in
  List.map test [
    "int -> bool", TyFun (TyInt, TyBool);
    "int -> bool -> ?", TyFun (TyInt, TyFun (TyBool, TyDyn));
    "(int -> bool) -> ?", TyFun (TyFun (TyInt, TyBool), TyDyn);
    "(int -> bool) -> ? -> int", TyFun (TyFun (TyInt, TyBool), TyFun (TyDyn, TyInt));
    "'a -> 'b", TyFun (TyVar 1, TyVar 2);
    "'a -> 'b", TyFun (TyVar 4, TyVar 2);
    "('a -> 'b) -> 'a", TyFun (TyFun (TyVar 2, TyVar 4), TyVar 2);
  ]

module GTLC = struct
  open Pp.GTLC

  let test_pp_program =
    let test (e) =
      e >:: fun ctxt ->
        assert_equal ~ctxt:ctxt ~printer:id e @@ asprintf "%a" pp_program @@ parse (e ^ ";;")
    in
    List.map test [
      "fun (x: ?) -> fun (y: ?) -> fun (z: ?) -> z";
      "x (y z)";
      "x y z";
      "1 * 2 + 3 * 4";
      "(1 + 2) * (3 + 4)";
      "(fun (x: ?) -> x) (fun (y: ?) -> y)";
    ]

  let suite = [
    "test_pp_program">::: test_pp_program;
  ]
end

module CC = struct
  open Pp.CC
  open Syntax.CC

  let r = Utils.Error.dummy_range

  let test_pp_exp =
    let test (expected, f) =
      expected >:: fun ctxt ->
        assert_equal ~ctxt:ctxt ~printer:id expected @@ asprintf "%a" pp_exp f
    in
    let x, y, z = Var (r, "x", []), Var (r, "y", []), Var (r, "z", []) in
    List.map test [
      "x y z", AppExp (r, AppExp (r, x, y), z);
      "x (y z)", AppExp (r, x, AppExp (r, y, z));
      "x * y + z * x", BinOp (r, Plus, BinOp (r, Mult, x, y), BinOp (r, Mult, z, x));
      "(x + y) * (z + x)", BinOp (r, Mult, BinOp (r, Plus, x, y), BinOp (r, Plus, z, x));
      "(fun (x: ?) -> x): ? -> ? => ?",
      CastExp (r, FunExp (r, "x", TyDyn, x), TyFun (TyDyn, TyDyn), TyDyn, Pos);
      "x: int => ?", CastExp (r, x, TyInt, TyDyn, Pos);
      "x: int => ? => bool", CastExp (r, CastExp (r, x, TyInt, TyDyn, Pos), TyDyn, TyBool, Pos);
      "(fun (x: ?) -> x) (fun (y: ?) -> y)",
      AppExp (r, FunExp (r, "x", TyDyn, x), FunExp (r, "y", TyDyn, y));
      "x y: int => ?", CastExp (r, AppExp (r, x, y), TyInt, TyDyn, Pos);
      "x (y: int => ?)", AppExp (r, x, CastExp (r, y, TyInt, TyDyn, Pos));
    ]

  let test_pp_value =
    let test (expected, v) =
      expected >:: fun ctxt ->
        assert_equal ~ctxt:ctxt ~printer:id expected @@ asprintf "%a" pp_value v
    in
    let b, i = BConst (r, true), IConst (r, 123) in
    List.map test [
      "true", b;
      "123", i;
      "<fun>", FunExp (r, "x", TyDyn, i);
      "<wrapped_fun>",
      CastExp (r, FunExp (r, "x", TyDyn, i), TyFun (TyInt, TyDyn), TyFun (TyDyn, TyDyn), Pos);
      "<fun>: ? -> ? => ?",
      CastExp (r, FunExp (r, "x", TyDyn, CastExp (r, i, TyInt, TyDyn, Pos)), TyFun (TyDyn, TyDyn), TyDyn, Pos);
    ]

  let suite = [
    "test_pp_exp">::: test_pp_exp;
    "test_pp_value">::: test_pp_value;
  ]
end

let suite = [
  "test_pp_ty">::: test_pp_ty;
  "test_pp_ty2">::: test_pp_ty2;
  "test_GTLC">::: GTLC.suite;
  "test_CC">::: CC.suite;
]
