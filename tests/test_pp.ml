open Format

open OUnit2

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
  ]

module GTLC = struct
  open Pp.GTLC

  let test_pp_exp =
    let test (e) =
      e >:: fun ctxt ->
        assert_equal ~ctxt:ctxt ~printer:id e @@ asprintf "%a" pp_exp @@ parse (e ^ ";;")
    in
    List.map test [
      "fun (x: ?) -> fun (y: ?) -> fun (z: ?) -> z";
      "x (y z)";
      "x y z";
      "1 * 2 + 3 * 4";
      "(1 + 2) * (3 + 4)";
    ]

  let suite = [
    "test_pp_exp">::: test_pp_exp;
  ]
end

module CC = struct
  open Pp.CC
  open Syntax.CC

  let test_pp_exp =
    let test (expected, f) =
      expected >:: fun ctxt ->
        assert_equal ~ctxt:ctxt ~printer:id expected @@ asprintf "%a" pp_exp f
    in
    let x, y, z = Var "x", Var "y", Var "z" in
    List.map test [
      "x y z", AppExp (AppExp (x, y), z);
      "x (y z)", AppExp (x, AppExp (y, z));
      "x * y + z * x", BinOp (Plus, BinOp (Mult, x, y), BinOp (Mult, z, x));
      "(x + y) * (z + x)", BinOp (Mult, BinOp (Plus, x, y), BinOp (Plus, z, x));
      "(x: int => ?)", CastExp (x, TyInt, TyDyn);
      "((x: int => ?): ? => bool)", CastExp (CastExp (x, TyInt, TyDyn), TyDyn, TyBool);
    ]

  let test_pp_value =
    let test (expected, v) =
      expected >:: fun ctxt ->
        assert_equal ~ctxt:ctxt ~printer:id expected @@ asprintf "%a" pp_value v
    in
    let b, i = BConst true, IConst 123 in
    List.map test [
      "true", b;
      "123", i;
      "<fun>", FunExp ("x", TyDyn, i);
      "<fun>: int -> ? => ? -> ?",
      CastExp (FunExp ("x", TyDyn, i), TyFun (TyInt, TyDyn), TyFun (TyDyn, TyDyn));
      "<fun>: ? -> ? => ?",
      CastExp (FunExp ("x", TyDyn, CastExp (i, TyInt, TyDyn)), TyFun (TyDyn, TyDyn), TyDyn);
    ]

  let suite = [
    "test_pp_exp">::: test_pp_exp;
    "test_pp_value">::: test_pp_value;
  ]
end

let suite = [
  "test_pp_ty">::: test_pp_ty;
  "test_GTLC">::: GTLC.suite;
  "test_CC">::: CC.suite;
]
