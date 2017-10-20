open Format
open Utils.Error

exception Blame of range * Syntax.CC.polarity

let pp_sep ppf () = fprintf ppf ", "

let pp_substitution ppf (x, u) =
  fprintf ppf "[%a :-> %a]"
    Pp.pp_ty (TyVar x)
    Pp.pp_ty u

let pp_substitutions ppf ss =
  pp_print_list pp_substitution ppf ss ~pp_sep:pp_sep
