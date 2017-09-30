open Syntax

exception Type_error of string

val fresh_gparam : unit -> ty
val fresh_tyvar : unit -> ty

type substitution = tyvar * ty
type substitutions = substitution list
val subst_type : substitutions -> ty -> ty

module GTLC : sig
  open Syntax.GTLC

  val type_of_program : tysc Environment.t -> program -> (program * ty)

  val translate : tysc Environment.t -> program -> (CC.program * ty)
end

module CC : sig
  open Syntax.CC

  val type_of_program : tysc Environment.t -> program -> ty

  val subst_exp : substitutions -> exp -> exp
end
