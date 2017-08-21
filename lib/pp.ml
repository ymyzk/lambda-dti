open Format

open Syntax

exception Syntax_error

let with_paren flag ppf_e ppf e =
  fprintf ppf (if flag then "(%a)" else "%a") ppf_e e

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
      (with_paren (gt_ty u u1) pp_ty) u1
      pp_ty u2

let gt_binop op1 op2 = match op1, op2 with
  | (Plus | Mult), Lt
  | Mult, Plus -> true
  | _ -> false

let gte_binop op1 op2 =
  if op1 = op2 then true else gt_binop op1 op2

let pp_binop ppf op =
  pp_print_string ppf begin
    match op with
    | Plus -> "+"
    | Mult -> "*"
    | Lt -> "<"
  end

module GTLC = struct
  open Syntax.GTLC

  let gt_exp e1 e2 = match e1, e2 with
    | (Var _ | IConst _ | BConst _ | AppExp _ | BinOp _), FunExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) -> gt_binop op1 op2
    | (Var _ | IConst _ | BConst _ | AppExp _), BinOp _ -> true
    | (Var _ | IConst _ | BConst _), AppExp _ -> true
    | _ -> false

  let gte_exp e1 e2 = match e1, e2 with
    | FunExp _, FunExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) when op1 = op2 -> true
    | AppExp _, AppExp _ -> true
    | _ -> gt_exp e1 e2

  let rec pp_exp ppf = function
    | Var (_, x) -> pp_print_string ppf x
    | BConst (_, b) -> pp_print_bool ppf b
    | IConst (_, i) -> pp_print_int ppf i
    | BinOp (_, op, e1, e2) as e ->
      fprintf ppf "%a %a %a"
        (with_paren (gt_exp e e1) pp_exp) e1
        pp_binop op
        (with_paren (gt_exp e e2) pp_exp) e2
    | FunExp (_, x1, u1, e) ->
      fprintf ppf "fun (%s: %a) -> %a"
        x1
        pp_ty u1
        pp_exp e
    | AppExp (_, e1, e2) as e ->
      fprintf ppf "%a %a"
        (with_paren (gt_exp e e1) pp_exp) e1
        (with_paren (gte_exp e e2) pp_exp) e2
end

module CC = struct
  open Syntax.CC

  let gt_exp f1 f2 = match f1, f2 with
    | (Var _ | IConst _ | BConst _ | AppExp _ | BinOp _ | CastExp _), FunExp _ -> true
    | (Var _ | IConst _ | BConst _ | AppExp _ | BinOp _), CastExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) -> gt_binop op1 op2
    | (Var _ | IConst _ | BConst _ | AppExp _), BinOp _ -> true
    | (Var _ | IConst _ | BConst _), AppExp _ -> true
    | _ -> false


  let gte_exp f1 f2 = match f1, f2 with
    | FunExp _, FunExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) when op1 = op2 -> true
    | AppExp _, AppExp _ -> true
    | CastExp _, CastExp _ -> true
    | _ -> gt_exp f1 f2

  let rec pp_exp ppf = function
    | Var (_, x) -> pp_print_string ppf x
    | BConst (_, b) -> pp_print_bool ppf b
    | IConst (_, i) -> pp_print_int ppf i
    | BinOp (_, op, f1, f2) as f ->
      fprintf ppf "%a %a %a"
        (with_paren (gt_exp f f1) pp_exp) f1
        pp_binop op
        (with_paren (gt_exp f f2) pp_exp) f2
    | FunExp (_, x1, u1, f) ->
      fprintf ppf "fun (%s: %a) -> %a"
        x1
        pp_ty u1
        pp_exp f
    | AppExp (_, f1, f2) as f ->
      fprintf ppf "%a %a"
        (with_paren (gt_exp f f1) pp_exp) f1
        (with_paren (gte_exp f f2) pp_exp) f2
    | CastExp (_, f1, u1, u2) as f ->
      match f1 with
      | CastExp (_, _, _, u1') when u1 = u1' ->
        fprintf ppf "%a => %a"
          (with_paren (gt_exp f f1) pp_exp) f1
          pp_ty u2
      | CastExp _ ->
        raise Syntax_error
      | _ ->
        fprintf ppf "%a: %a => %a"
          (with_paren (gt_exp f f1) pp_exp) f1
          pp_ty u1
          pp_ty u2

  let rec pp_value ppf v =
    assert (is_value v);
    match v with
    | BConst _
    | IConst _ -> pp_exp ppf v
    | FunExp _ -> pp_print_string ppf "<fun>"
    | CastExp (_, v, u1, u2) ->
      fprintf ppf "%a: %a => %a"
        pp_value v
        pp_ty u1
        pp_ty u2
    | _ -> raise Syntax_error
end
