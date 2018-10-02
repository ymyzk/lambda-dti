open Syntax

exception Type_error of string

val fresh_tyvar : unit -> ty

val is_equal : ty -> ty -> bool

(* [X:->u] *)
type substitution = tyvar * ty
(* if S = [X1:->X2], [X2:->u1], then S(X1)=u1 *)
type substitutions = substitution list
val subst_type : substitutions -> ty -> ty

module ITGL : sig
  open Syntax.ITGL

  val type_of_program : tysc Environment.t -> program -> (tysc Environment.t * program * ty)

  val normalize : tysc Environment.t -> program -> ty -> (tysc Environment.t * program * ty)
  val normalize_type : ty -> ty

  val translate : tysc Environment.t -> program -> (CC.program * ty)
end

module CC : sig
  open Syntax.CC

  val type_of_program : tysc Environment.t -> program -> ty
end
