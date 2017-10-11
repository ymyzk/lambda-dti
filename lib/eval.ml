open Format
open Syntax
open Syntax.CC
open Utils.Error

exception Blame of range

type substitution = tyvar * ty
type substitutions = substitution list

(* s(u) *)
(* TODO: Merge with Typing *)
let subst_tv_type s u =
  let rec subst_tv (x, u as s) = function
    | TyFun (u1, u2) -> TyFun (subst_tv s u1, subst_tv s u2)
    | TyVar x' when x = x' -> u
    | _ as u -> u
  in
  List.fold_left (fun u -> fun s0 -> subst_tv s0 u) u s

(* s(e) *)
let rec subst_tv_exp s f =
  map_exp (fun u -> subst_tv_type s u) (subst_tv_exp s) f

let pp_sep ppf () = fprintf ppf ", "

let pp_substitution ppf (x, u) =
  fprintf ppf "[%a :-> %a]"
    Pp.pp_ty (TyVar x)
    Pp.pp_ty u

let pp_substitutions ppf ss =
  pp_print_list pp_substitution ppf ss ~pp_sep:pp_sep
