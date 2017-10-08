open Format
open Syntax
open Syntax.CC
open Utils.Error

exception Blame of range

(* Substitution for gradual type parameters *)

type substitution = typaram * ty
type substitutions = substitution list

(* s(u) *)
let subst_gtp_type s u =
  let rec subst_gtp (a, u as s) = function
    | TyFun (u1, u2) -> TyFun (subst_gtp s u1, subst_gtp s u2)
    | TyGParam a' when a = a' -> u
    | _ as u -> u
  in
  List.fold_left (fun u -> fun s0 -> subst_gtp s0 u) u s

(* s(e) *)
let rec subst_gtp_exp s e =
  map_exp (fun u -> subst_gtp_type s u) (subst_gtp_exp s) e

let pp_sep ppf () = fprintf ppf ", "

let pp_substitution ppf (a, u) =
  fprintf ppf "['a%d :-> %a]" a Pp.pp_ty u

let pp_substitutions ppf ss =
  pp_print_list pp_substitution ppf ss ~pp_sep:pp_sep
