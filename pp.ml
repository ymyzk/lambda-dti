open Format

open Syntax

exception Syntax_error

let with_paren gt ppf_e e_up ppf e =
  fprintf ppf (if gt e_up e then "(%a)" else "%a") ppf_e e

let gt_ty (_: ty) = function
  | TyFun _ -> true
  | _ -> false

let rec pp_ty ppf = function
  | TyDyn -> pp_print_string ppf "?"
  | TyGParam a -> fprintf ppf "'a%d" a
  | TySParam b -> fprintf ppf "'b%d" b
  | TyVar x -> fprintf ppf "'x%d" x
  | TyInt -> pp_print_string ppf "int"
  | TyBool -> pp_print_string ppf "bool"
  | TyFun (u1, u2) as u ->
      fprintf ppf "%a -> %a"
        (with_paren gt_ty pp_ty u) u1
        pp_ty u2

let pp_binop ppf op =
  pp_print_string ppf begin
    match op with
    | Plus -> "+"
    | Mult -> "*"
    | Lt -> "<"
  end

module GTLC = struct
  open Syntax.GTLC

  let gt_exp e_up e = match e_up, e with
    | AppExp _, _ -> true
    | BinOp (Mult, _, _), BinOp (Plus, _, _) -> true
    | BinOp (Mult, _, _), BinOp (Lt, _, _) -> true
    | BinOp (Plus, _, _), BinOp (Lt, _, _) -> true
    | _ -> false

  let rec pp_exp ppf = function
    | Var x -> pp_print_string ppf x
    | BConst b -> pp_print_bool ppf b
    | IConst i -> pp_print_int ppf i
    | BinOp (op, e1, e2) as e ->
        fprintf ppf "%a %a %a"
          (with_paren gt_exp pp_exp e) e1
          pp_binop op
          (with_paren gt_exp pp_exp e) e2
    | FunExp (x1, u1, e) ->
        fprintf ppf "fun (%s: %a) -> %a"
          x1
          pp_ty u1
          pp_exp e
    | AppExp (e1, e2) as e ->
        fprintf ppf "%a %a"
          pp_exp e1
          (with_paren gt_exp pp_exp e) e2
end

module CC = struct
  open Syntax.CC

  let rec pp_exp ppf = function
    | Var x -> pp_print_string ppf x
    | BConst b -> pp_print_bool ppf b
    | IConst i -> pp_print_int ppf i
    | BinOp (op, f1, f2) ->
        fprintf ppf "(%a %a %a)"
          pp_exp f1
          pp_binop op
          pp_exp f2
    | FunExp (x1, u1, f) ->
        fprintf ppf "fun (%s: %a) -> %a"
          x1
          pp_ty u1
          pp_exp f
    | AppExp (f1, f2) ->
        fprintf ppf "((%a) (%a))"
          pp_exp f1
          pp_exp f2
    | CastExp (f, u1, u2) ->
        fprintf ppf "(%a: %a => %a)"
          pp_exp f
          pp_ty u1
          pp_ty u2

  let rec pp_value ppf v =
    assert (is_value v);
    match v with
    | BConst _
    | IConst _ -> pp_exp ppf v
    | FunExp _ -> pp_print_string ppf "<fun>"
    | CastExp (v, u1, u2) ->
        fprintf ppf "%a: %a => %a"
          pp_value v
          pp_ty u1
          pp_ty u2
    | _ -> raise Syntax_error
end
