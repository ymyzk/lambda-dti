open Syntax

exception Type_error of string

val fresh_gparam : unit -> ty
val fresh_tyvar : unit -> ty

module GTLC : sig
  open Syntax.GTLC

  type substitution = tyvar * ty
  type substitutions = substitution list
  val subst_type : tyvar -> ty -> ty -> ty
  val subst_type_substitutions : ty -> substitutions -> ty
  val subst_exp_substitutions : exp -> substitutions -> exp

  val generate_typaram_subst : ty list -> exp -> substitutions

  val type_of_exp : tysc Environment.t -> exp -> (ty * substitutions * ty list)

  val translate : tysc Environment.t -> exp -> (CC.exp * ty)
end

module CC : sig
  open Syntax.CC

  val type_of_exp : tysc Environment.t -> exp -> ty
end
