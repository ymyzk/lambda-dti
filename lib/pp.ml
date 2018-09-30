open Format

open Syntax

exception Syntax_error

let with_paren flag ppf_e ppf e =
  fprintf ppf (if flag then "(%a)" else "%a") ppf_e e

let rec gt_ty (u1: ty) u2 = match u1, u2 with
  | TyVar ({ contents = Some u1 }), u2
  | u1, TyVar ({ contents = Some u2 }) -> gt_ty u1 u2
  | _, TyFun _ -> true
  | _ -> false

let rec pp_ty ppf = function
  | TyDyn -> pp_print_string ppf "?"
  | TyVar ({ contents = None } as r) -> fprintf ppf "'x%d" (2 * Obj.magic r) (* TODO: OK? *)
  | TyVar ({ contents = Some u }) -> pp_ty ppf u
  | TyInt -> pp_print_string ppf "int"
  | TyBool -> pp_print_string ppf "bool"
  | TyUnit -> pp_print_string ppf "unit"
  | TyFun (u1, u2) as u ->
    fprintf ppf "%a -> %a"
      (with_paren (gt_ty u u1) pp_ty) u1
      pp_ty u2

let pp_ty2 ppf u =
  let tyvars = ref [] in
  let pp_tyvar ppf x =
    let rec index_of_tyvar pos = function
      | [] -> tyvars := !tyvars @ [x]; pos
      | y :: rest -> if x == y then pos else index_of_tyvar (pos + 1) rest
    in
    let pp_tyvar_of_index ppf i =
      let j = i / 26 in
      let k = i mod 26 in
      let s = String.make 1 @@ char_of_int @@ (int_of_char 'a') + k in
      let t = if j = 0 then "" else string_of_int j in
      fprintf ppf "'%s%s" s t
    in
    pp_tyvar_of_index ppf @@ index_of_tyvar 0 !tyvars
  in
  let rec pp_ty ppf = function
    | TyDyn -> pp_print_string ppf "?"
    | TyVar ({ contents = Some u }) -> pp_ty ppf u
    | TyVar x -> pp_tyvar ppf x
    | TyInt -> pp_print_string ppf "int"
    | TyBool -> pp_print_string ppf "bool"
    | TyUnit -> pp_print_string ppf "unit"
    | TyFun (u1, u2) as u ->
      fprintf ppf "%a -> %a"
        (with_paren (gt_ty u u1) pp_ty) u1
        pp_ty u2
  in pp_ty ppf u

let gt_binop op1 op2 = match op1, op2 with
  | (Plus | Minus | Mult | Div | Mod), (Eq | Neq | Lt | Lte | Gt | Gte)
  | (Mult | Div | Mod), (Plus | Minus) -> true
  | _ -> false

let gte_binop op1 op2 = match op1, op2 with
  | (Eq | Neq | Lt | Lte | Gt | Gte), (Eq | Neq | Lt | Lte | Gt | Gte)
  | (Mult | Div | Mod), (Mult | Div | Mod)
  | (Plus | Minus), (Plus | Minus) -> true
  | _ -> gt_binop op1 op2

let pp_binop ppf op =
  pp_print_string ppf begin
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "mod"
    | Eq -> "="
    | Neq -> "<>"
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

  let pp_constr ppf = function
    | CEqual (u1, u2) ->
      fprintf ppf "%a =.= %a" pp_ty u1 pp_ty u2
    | CConsistent (u1, u2) ->
      fprintf ppf "%a ~.~ %a" pp_ty u1 pp_ty u2

  let gt_exp e1 e2 = match e1, e2 with
    | (Var _ | IConst _ | BConst _ | UConst _ | AscExp _ | AppExp _ | BinOp _ | IfExp _), (LetExp _ | FunExp _ | FixExp _) -> true
    | (Var _ | IConst _ | BConst _ | UConst _ | AscExp _ | AppExp _ | BinOp _), IfExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) -> gt_binop op1 op2
    | (Var _ | IConst _ | BConst _ | UConst _ | AscExp _ | AppExp _), BinOp _ -> true
    | (Var _ | IConst _ | BConst _ | UConst _ | AscExp _), AppExp _ -> true
    | _ -> false

  let gte_exp e1 e2 = match e1, e2 with
    | LetExp _, LetExp _ -> true
    | FunExp _, FunExp _ -> true
    | FixExp _, FixExp _ -> true
    | IfExp _, IfExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) when op1 = op2 -> true
    | AppExp _, AppExp _ -> true
    | _ -> gt_exp e1 e2

  let rec pp_exp ppf = function
    | Var (_, x, ys) -> pp_print_var ppf (x, !ys)
    | BConst (_, b) -> pp_print_bool ppf b
    | IConst (_, i) -> pp_print_int ppf i
    | UConst _ -> pp_print_string ppf "()"
    | BinOp (_, op, e1, e2) as e ->
      fprintf ppf "%a %a %a"
        (with_paren (gt_exp e e1) pp_exp) e1
        pp_binop op
        (with_paren (gt_exp e e2) pp_exp) e2
    | AscExp (_, e, u) ->
      fprintf ppf "(%a : %a)"
        pp_exp e
        pp_ty u
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
    | FixExp (_, x, y, u1, u2, e) ->
      fprintf ppf "fix %s (%s: %a): %a = %a"
        x
        y
        pp_ty u1
        pp_ty u2
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
    | (Var _ | IConst _ | BConst _ | UConst _ | AppExp _ | BinOp _ | IfExp _ | CastExp _), (LetExp _ | FunExp _ | FixExp _) -> true
    | (Var _ | IConst _ | BConst _ | UConst _ | AppExp _ | BinOp _ | IfExp _), CastExp _ -> true
    | (Var _ | IConst _ | BConst _ | UConst _ | AppExp _ | BinOp _), IfExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) -> gt_binop op1 op2
    | (Var _ | IConst _ | BConst _ | UConst _ | AppExp _), BinOp _ -> true
    | (Var _ | IConst _ | BConst _ | UConst _), AppExp _ -> true
    | _ -> false

  let gte_exp f1 f2 = match f1, f2 with
    | (LetExp _ | FunExp _ | FixExp _), (LetExp _ | FunExp _ | FixExp _) -> true
    | IfExp _, IfExp _ -> true
    | BinOp (_, op1, _, _), BinOp (_, op2, _, _) when op1 = op2 -> true
    | AppExp _, AppExp _ -> true
    | CastExp _, CastExp _ -> true
    | _ -> gt_exp f1 f2

  let rec pp_exp ppf = function
    | Var (_, x, ys) -> pp_print_var ppf (x, ys)
    | BConst (_, b) -> pp_print_bool ppf b
    | IConst (_, i) -> pp_print_int ppf i
    | UConst _ -> pp_print_string ppf "()"
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
    | FixExp (_, x, y, u1, u2, f) ->
      fprintf ppf "fix %s (%s: %a): %a = %a"
        x
        y
        pp_ty u1
        pp_ty u2
        pp_exp f
    | AppExp (_, f1, f2) as f ->
      fprintf ppf "%a %a"
        (with_paren (gt_exp f f1) pp_exp) f1
        (with_paren (gte_exp f f2) pp_exp) f2
    | CastExp (_, f1, u1, u2, _) as f ->
      begin match f1 with
      | CastExp (_, _, _, u1', _) when u1 = u1' ->
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

  let pp_program ppf = function
    | Exp e -> pp_exp ppf e
    | LetDecl (x, xs, f) ->
      fprintf ppf "let %s = %a%a"
        x
        pp_let_tyabses xs
        pp_exp f

  let pp_tag ppf t = pp_ty ppf @@ tag_to_ty t

  let rec pp_value ppf = function
    | BoolV b -> pp_print_bool ppf b
    | IntV i -> pp_print_int ppf i
    | UnitV -> pp_print_string ppf "()"
    | FunV _ -> pp_print_string ppf "<fun>"
    | Tagged (t, v) ->
      fprintf ppf "%a: %a => ?"
        pp_value v
        pp_tag t
end
