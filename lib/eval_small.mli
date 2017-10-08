open Eval
open Syntax
open Syntax.CC

val eval : ?debug:bool -> exp -> (exp * substitutions)
val eval_program : ?debug:bool -> program -> (exp * substitutions)
