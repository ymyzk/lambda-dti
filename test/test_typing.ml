open Format

open OUnit2

open Lambda_rti
open Syntax
open Typing

let id x = x

module GTLC = struct
  open Typing.GTLC

  let parse str =
    Parser.toplevel Lexer.main @@ Lexing.from_string str

  let test_type_of_program =
    let tyenv = Environment.empty in
    let test (program, expected) =
      program >:: fun ctxt ->
        let e = parse @@ program ^ ";;" in
        let _, _, u = Typing.GTLC.type_of_program tyenv e in
        let u = Typing.GTLC.normalize_type u in
        assert_equal ~ctxt:ctxt ~printer:id expected @@ asprintf "%a" Pp.pp_ty2 u
    in
    List.map test [
      "1", "int";
      "true", "bool";
      "1 + 2 + 3", "int";
      "(true : ?)", "?";
      "((true : ?) : int)", "int";
      "fun x -> x + 1", "int -> int";
      "fun x -> x", "'a -> 'a";
      "fun (x:?) -> x + 2", "? -> int";
      "(fun (x:?) -> x + 2) 3", "int";
      "(fun (x:?) -> x + 2) true", "int";
      "(fun (x:?) -> x 2) (fun y -> true)", "?";
      "(fun (x:?) -> x) (fun y -> y)", "?";
      "(fun (x:?) -> x 2) (fun y -> y)", "?";
      "fun x y -> x y", "('a -> 'b) -> 'a -> 'b";
      "fun y z -> z (z y)", "'a -> ('a -> 'a) -> 'a";
      "fun x y z -> x z (y z)", "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c";
      "fun x y -> x", "'a -> 'b -> 'a";
      "let id x = x", "'a -> 'a";
      "let dynid (x:?) = x", "? -> ?";
      "let succ x = x + 1", "int -> int";
      "let id x = x in let did (x:?) = x in let succ x = x + 1 in (fun (x:?) -> x 1) (id (did succ))", "?";
      "let id x = x in let did (x:?) = x in let succ x = x + 1 in (fun (x:?) -> x true) (id (did succ))", "?";
      "let rec f x (y:bool) z: int = 1 in f", "'a -> bool -> 'b -> int";
    ]

  let test_type_of_program_errors =
    let tyenv = Environment.empty in
    let test program =
      program >:: fun ctxt ->
        let e = parse @@ program ^ ";;" in
        (* NOTE: Currently we do not care about contents of the exception *)
        let message = begin
          try
            ignore @@ type_of_program tyenv e;
            Some (asprintf "Type_error is not raised: '%s'" program)
          with
          | Type_error _ ->
            (* OK *)
            None
          | _ ->
            Some (asprintf "Unexpected exception is raised: '%s'" program)
        end in
        match message with
        | None -> ()
        | Some m -> assert_failure m
    in
    List.map test [
      "1 + true";
      "true * false";
      "1 < false";
      "if 0 then true else false";
      "if true then 1 else false";
      "x";
    ]

  let suite = [
    "test_type_of_program">::: test_type_of_program;
    "test_type_of_program_errors">::: test_type_of_program_errors;
  ]
end

let suite = [
  "test_GTLC">::: GTLC.suite;
]
