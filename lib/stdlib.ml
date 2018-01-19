open Syntax
open Syntax.CC

exception Stdlib_bug of string

let env, tyenv = Environment.empty, Environment.empty

let is_some_type = tysc_of_ty @@ TyFun (TyDyn, TyBool)
let is_some u = FunV (fun _ -> function
    | Tagged (t, _) when u = tag_to_ty t -> BoolV true
    | Tagged _ -> BoolV false
    |_ -> raise @@ Stdlib_bug "untagged value"
  )

let implementations = [
  "is_bool", [], is_some TyBool, is_some_type;
  "is_int", [], is_some TyInt, is_some_type;
  "is_unit", [], is_some TyUnit, is_some_type;
  "is_fun", [], is_some (TyFun (TyDyn, TyDyn)), is_some_type;
]

let env, tyenv =
  List.fold_left
    (fun (env, tyenv) (x, xs, v, u) ->
       Environment.add x (xs, v) env,  Environment.add x u tyenv)
    (env, tyenv)
    implementations

let implementations = [
  "let ignore x = ();;"
]

let env, tyenv =
 List.fold_left
    (fun (env, tyenv) str ->
      let e = Parser.toplevel Lexer.main @@ Lexing.from_string str in
      let tyenv, e, u = Typing.GTLC.type_of_program tyenv e in
      let tyenv = Typing.GTLC.normalize_tyenv tyenv in
      let e = Typing.GTLC.normalize_program e in
      let _ = Typing.GTLC.normalize_type u in
      let f, _ = Typing.GTLC.translate tyenv e in
      let _ = Typing.CC.type_of_program tyenv f in
      let env, _, _ = Eval.eval_program env f in
      env, tyenv)
    (env, tyenv)
    implementations

let pervasives = env, tyenv
