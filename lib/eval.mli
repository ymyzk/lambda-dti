open Syntax
open Syntax.CC
open Utils.Error

exception Blame of range * polarity

exception Eval_bug of string

val eval_program : ?debug:bool -> (tyvar list * value) Environment.t -> program -> (tyvar list * value) Environment.t * id * value
