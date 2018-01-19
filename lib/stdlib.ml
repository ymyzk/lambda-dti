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

let pervasives = env, tyenv
