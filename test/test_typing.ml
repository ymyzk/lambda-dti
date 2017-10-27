open Format

open OUnit2

open Lambda_rti
open Syntax

let id x = x

module GTLC = struct
  let parse str =
    Parser.toplevel Lexer.main @@ Lexing.from_string str

  let test_type_of_program =
    let tyenv = Environment.empty in
    let test (program, expected) =
      program >:: fun ctxt ->
        let e = parse @@ program ^ ";;" in
        let _, _, u = Typing.GTLC.type_of_program tyenv e in
        assert_equal ~ctxt:ctxt ~printer:id expected @@ asprintf "%a" Pp.pp_ty2 u
    in
    List.map test [
      "1", "int";
      "true", "bool";
      "1 + 2 + 3", "int";
      "fun x -> x + 1", "int -> int";
      "fun x -> x", "'a -> 'a";
      "fun (x:?) -> x + 2", "? -> int";
      "(fun (x:?) -> x + 2) 3", "int";
      "(fun (x:?) -> x + 2) true", "int";
      "(fun (x:?) -> x 2) (fun y -> true)", "?";
      "(fun (x:?) -> x) (fun y -> y)", "?";
      "(fun (x:?) -> x 2) (fun y -> y)", "?";
      "let succ x = x + 1", "int -> int";
    ]

  let suite = [
    "test_type_of_program">::: test_type_of_program;
  ]
end

let suite = [
  "test_GTLC">::: GTLC.suite;
]
