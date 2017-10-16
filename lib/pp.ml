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
  | TyVar x -> fprintf ppf "'x%d" x
  | TyInt -> pp_print_string ppf "int"
  | TyBool -> pp_print_string ppf "bool"
  | TyFun (u1, u2) as u ->
    fprintf ppf "%a -> %a"
      (with_paren (gt_ty u u1) pp_ty) u1
      pp_ty u2

let gt_binop op1 op2 = match op1, op2 with
  | (Plus | Minus | Mult | Div), (Lt | Lte | Gt | Gte)
  | (Mult | Div), (Plus | Minus) -> true
  | _ -> false

let gte_binop op1 op2 = match op1, op2 with
  | (Lt | Lte | Gt | Gte), (Lt | Lte | Gt | Gte)
  | (Mult | Div), (Mult | Div)
  | (Plus | Minus), (Plus | Minus) -> true
  | _ -> gt_binop op1 op2

let pp_binop ppf op =
  pp_print_string ppf begin
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Lt -> "<"
    | Lte -> "<="
    | Gt -> ">"
    | Gte -> ">="
  end

let pp_print_var ppf (x, ys) =
  if List.length ys = 0 then
    fprintf ppf "%s" x
  else
    let pp_sep ppf () = fprintf ppf "," in
    let pp_list ppf types = pp_print_list pp_ty ppf types ~pp_sep:pp_sep in
    fprintf ppf "%s[%a]"
      x
      pp_list ys

let pp_let_tyabses ppf tyvars =
  if List.length tyvars = 0 then
    fprintf ppf ""
  else
    let pp_sep ppf () = fprintf ppf "," in
    let pp_list ppf types = pp_print_list pp_ty ppf types ~pp_sep:pp_sep in
    fprintf ppf "fun %a -> " pp_list @@ List.map (fun x -> TyVar x) tyvars

module GTLC = struct
  open Syntax.GTLC

  let gt_exp e1 e2 = match e1, e2 with
    | (Var _ | IConst _ | BConst _ | AppExp _ | BinOp _ | IfExp _), (LetExp _ | FunExp _) -> true
    | (Var _ | IConst _ | BConst _ | AppExp _ | BinOp _), IfExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) -> gt_binop op1 op2
    | (Var _ | IConst _ | BConst _ | AppExp _), BinOp _ -> true
    | (Var _ | IConst _ | BConst _), AppExp _ -> true
    | _ -> false

  let gte_exp e1 e2 = match e1, e2 with
    | LetExp _, LetExp _ -> true
    | FunExp _, FunExp _ -> true
    | IfExp _, IfExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) when op1 = op2 -> true
    | AppExp _, AppExp _ -> true
    | _ -> gt_exp e1 e2

  let rec pp_exp ppf = function
    | Var (_, x, ys) -> pp_print_var ppf (x, !ys)
    | BConst (_, b) -> pp_print_bool ppf b
    | IConst (_, i) -> pp_print_int ppf i
    | BinOp (_, op, e1, e2) as e ->
      fprintf ppf "%a %a %a"
        (with_paren (gt_exp e e1) pp_exp) e1
        pp_binop op
        (with_paren (gt_exp e e2) pp_exp) e2
    | IfExp (_, e1, e2, e3) as e ->
      fprintf ppf "if %a then %a else %a"
        (with_paren (gt_exp e e1) pp_exp) e1
        (with_paren (gt_exp e e2) pp_exp) e2
        (with_paren (gt_exp e e3) pp_exp) e3
    | FunExp (_, x1, u1, e) ->
      fprintf ppf "fun (%s: %a) -> %a"
        x1
        pp_ty u1
        pp_exp e
    | AppExp (_, e1, e2) as e ->
      fprintf ppf "%a %a"
        (with_paren (gt_exp e e1) pp_exp) e1
        (with_paren (gte_exp e e2) pp_exp) e2
    | LetExp (_, x, xs, e1, e2) as e ->
      fprintf ppf "let %s = %a%a in %a"
        x
        pp_let_tyabses !xs
        (with_paren (gt_exp e e1) pp_exp) e1
        (with_paren (gte_exp e e2) pp_exp) e2

  let pp_program ppf = function
    | Exp e -> pp_exp ppf e
    | LetDecl (x, xs, e) ->
      fprintf ppf "let %s = %a%a"
        x
        pp_let_tyabses !xs
        pp_exp e
end

module CC = struct
  open Syntax.CC

  let gt_exp f1 f2 = match f1, f2 with
    | (Var _ | IConst _ | BConst _ | AppExp _ | BinOp _ | IfExp _ | CastExp _), (LetExp _ | FunExp _) -> true
    | (Var _ | IConst _ | BConst _ | AppExp _ | BinOp _ | IfExp _), CastExp _ -> true
    | (Var _ | IConst _ | BConst _ | AppExp _ | BinOp _), IfExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) -> gt_binop op1 op2
    | (Var _ | IConst _ | BConst _ | AppExp _), BinOp _ -> true
    | (Var _ | IConst _ | BConst _), AppExp _ -> true
    | _ -> false

  let gte_exp f1 f2 = match f1, f2 with
    | (LetExp _ | FunExp _), (LetExp _ | FunExp _) -> true
    | IfExp _, IfExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) when op1 = op2 -> true
    | AppExp _, AppExp _ -> true
    | CastExp _, CastExp _ -> true
    | _ -> gt_exp f1 f2

  let rec pp_exp ppf = function
    | Var (_, x, ys) -> pp_print_var ppf (x, ys)
    | BConst (_, b) -> pp_print_bool ppf b
    | IConst (_, i) -> pp_print_int ppf i
    | BinOp (_, op, f1, f2) as f ->
      fprintf ppf "%a %a %a"
        (with_paren (gt_exp f f1) pp_exp) f1
        pp_binop op
        (with_paren (gt_exp f f2) pp_exp) f2
    | IfExp (_, f1, f2, f3) as f ->
      fprintf ppf "if %a then %a else %a"
        (with_paren (gt_exp f f1) pp_exp) f1
        (with_paren (gt_exp f f2) pp_exp) f2
        (with_paren (gt_exp f f3) pp_exp) f3
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
      begin match f1 with
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
      end
    | LetExp (_, x, xs, f1, f2) as f ->
      fprintf ppf "let %s = %a%a in %a"
        x
        pp_let_tyabses xs
        (with_paren (gt_exp f f1) pp_exp) f1
        (with_paren (gte_exp f f2) pp_exp) f2
    | Hole -> raise Syntax_error

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

  let pp_program ppf = function
    | Exp e -> pp_exp ppf e
    | LetDecl (x, xs, f) ->
      fprintf ppf "let %s = %a%a"
        x
        pp_let_tyabses xs
        pp_exp f
end
