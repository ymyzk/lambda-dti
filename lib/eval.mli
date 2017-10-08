open Format
open Syntax
open Syntax.CC
open Utils.Error

exception Blame of range

(* [a:->u] *)
type substitution = typaram * ty
(* if S = [a1:->a2], [a2:->u1], then S(a1)=u1 *)
type substitutions = substitution list

val subst_gtp_type : substitutions -> ty -> ty
val subst_gtp_exp : substitutions -> exp -> exp

val pp_substitutions : formatter -> substitutions -> unit

val eval : ?debug:bool -> exp -> (exp * substitutions)
val eval_program : ?debug:bool -> program -> (exp * substitutions)
