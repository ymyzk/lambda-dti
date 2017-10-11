open Format
open Syntax
open Syntax.CC
open Utils.Error

exception Blame of range

(* [X:->u] *)
type substitution = tyvar * ty
(* if S = [X1:->X2], [X2:->u1], then S(X1)=u1 *)
type substitutions = substitution list

val subst_tv_type : substitutions -> ty -> ty
val subst_tv_exp : substitutions -> exp -> exp

val pp_substitutions : formatter -> substitutions -> unit
