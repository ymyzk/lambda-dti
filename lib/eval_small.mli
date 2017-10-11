open Syntax
open Syntax.CC
open Typing

val eval : ?debug:bool -> (tyvar list * exp) Environment.t -> exp -> (exp * substitutions)
val eval_program : ?debug:bool -> (tyvar list * exp) Environment.t -> program -> ((tyvar list * exp) Environment.t * id * exp * substitutions)
