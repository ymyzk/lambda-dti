open Eval
open Format
open Syntax
open Syntax.CC
open Typing
open Typing.CC

exception Decomposition_not_found
exception Reduce

(* s(c) *)
let rec subst_tv_context s = function
  | CTop -> CTop
  | CAppL (r, c, e) -> CAppL (r, subst_tv_context s c, subst_exp s e)
  | CAppR (r, e, c) -> CAppR (r, subst_exp s e, subst_tv_context s c)
  | CBinOpL (r, op, c, e) -> CBinOpL (r, op, subst_tv_context s c, subst_exp s e)
  | CBinOpR (r, op, e, c) -> CBinOpR (r, op, subst_exp s e, subst_tv_context s c)
  | CCast (r, c, u1, u2) -> CCast (r, subst_tv_context s c, subst_type s u1, subst_type s u2)

(* Substitution for variables *)

(* f[x:=v] *)
let rec subst_var (x: id) (xs: tyvar list) (v: exp) (f: exp): exp =
  assert (is_value v);
  let subst = subst_var x xs v in
  match f with
  | Var (_, y, ys) ->
    if x <> y then
      f
    else
      subst_exp (Utils.zip xs ys) v
  | IConst _
  | BConst _ as f -> f
  | BinOp (r, op, f1, f2) -> BinOp (r, op, subst f1, subst f2)
  | FunExp (r, y, u, f') as f ->
    if x = y then f else FunExp (r, y, u, subst f')
  | AppExp (r, f1, f2) -> AppExp (r, subst f1, subst f2)
  | CastExp (r, f1, u1, u2) -> CastExp (r, subst f1, u1, u2)
  | LetExp (r, y, ys, f1, f2) ->
    LetExp (r, y, ys, subst f1, if x = y then f2 else subst f2)
  | Hole as f -> f

(* Reduction *)

let reduce = function
  (* R_Beta *)
  | AppExp (_, FunExp (_, x, _, e), v) when is_value v ->
    subst_var x [] v e, None
  (* R_Op *)
  | BinOp (r, Plus, IConst (_, i1), IConst (_, i2)) ->
    IConst (r, i1 + i2), None
  | BinOp (r, Mult, IConst (_, i1), IConst (_, i2)) ->
    IConst (r, i1 * i2), None
  | BinOp (r, Lt, IConst (_, i1), IConst (_, i2)) ->
    BConst (r, i1 < i2), None
  (* R_IdBase *)
  | CastExp (_, v, TyBool, TyBool) when is_value v -> v, None
  | CastExp (_, v, TyInt, TyInt) when is_value v -> v, None
  (* R_IdStar *)
  | CastExp (_, v, TyDyn, TyDyn) when is_value v -> v, None
  (* R_AppCast *)
  | AppExp (_, CastExp (r, v1, TyFun (u11, u12), TyFun (u21, u22)), v2) when is_value v1 && is_value v2 ->
    CastExp (r, AppExp (r, v1, CastExp (r, v2, u21, u11)), u12, u22), None
  | CastExp (_, CastExp (r2, v, u1, u2), u2', u3) when is_value v && u2 = u2' ->
    begin match (u1, u2, u3) with
      (* R_Succeed *)
      | g1, TyDyn, g2 when is_ground g1 && is_ground g2 && g1 = g2 -> v, None
      (* R_InstBase *)
      | TyBool, TyDyn, TyVar x -> v, Some (x, TyBool)
      | TyInt, TyDyn, TyVar x -> v, Some (x, TyInt)
      (* R_InstArrow *)
      | TyFun (TyDyn, TyDyn), TyDyn, TyVar x ->
        let x1, x2 = Typing.fresh_tyvar (), Typing.fresh_tyvar () in
        CastExp (r2, v, TyFun (TyDyn, TyDyn), TyFun (x1, x2)), Some (x, TyFun (x1, x2))
      (* R_Fail *)
      | g1, TyDyn, g2 when is_ground g1 && is_ground g2 && g1 <> g2 ->
        raise @@ Blame (r2)
      | _ -> raise Reduce
    end
  (* R_Ground *)
  | CastExp (r, v, u, TyDyn) when u <> TyDyn && Some u <> ground_of_ty u ->
    begin match ground_of_ty u with
    | Some g ->
      CastExp (r, CastExp(r, v, u, g), g, TyDyn), None
    | None -> raise Not_found
    end
  (* R_Expand *)
  | CastExp (r, v, TyDyn, u) when u <> TyDyn && Some u <> ground_of_ty u ->
    begin match ground_of_ty u with
    | Some g ->
      CastExp (r, CastExp(r, v, TyDyn, g), g, u), None
    | None -> raise Not_found
    end
  (* R_LetP *)
  | LetExp (_, x, xs, v1, f2) when is_value v1 ->
    subst_var x xs v1 f2, None
  | _ ->
    raise Reduce

(* Evaluation *)

let rec fill_context = function
  | CTop, e -> e
  | CAppL (r, c, e2), e1 -> fill_context (c, AppExp (r, e1, e2))
  | CAppR (r, e1, c), e2 -> fill_context (c, AppExp (r, e1, e2))
  | CBinOpL (r, op, c, e2), e1 -> fill_context (c, BinOp (r, op, e1, e2))
  | CBinOpR (r, op, e1, c), e2 -> fill_context (c, BinOp (r, op, e1, e2))
  | CCast (r, c, u1, u2), e -> fill_context (c, CastExp (r, e, u1, u2))

exception Error of context * exp
exception Value

let rec exp_with exp_in_hole ppf =
  let exp = exp_with exp_in_hole in
  function
  | Var (_, x, ys) -> Pp.pp_print_var ppf (x, ys)
  | BConst (_, b) -> pp_print_bool ppf b
  | IConst (_, i) -> pp_print_int ppf i
  | BinOp (_, op, f1, f2) ->
    fprintf ppf "(%a %a %a)"
      exp f1
      Pp.pp_binop op
      exp f2
  | FunExp (_, x1, u1, f) ->
    fprintf ppf "fun (%s: %a) -> %a"
      x1
      Pp.pp_ty u1
      exp f
  | AppExp (_, f1, f2) ->
    fprintf ppf "((%a) (%a))"
      exp f1
      exp f2
  | CastExp (_, f, u1, u2) ->
    fprintf ppf "(%a: %a => %a)"
      exp f
      Pp.pp_ty u1
      Pp.pp_ty u2
  | LetExp (_, x, xs, f1, f2) ->
    fprintf ppf "let %s = %a%a in %a"
      x
      Pp.pp_let_tyabses xs
      exp f1
      exp f2
  | Hole ->
    fprintf ppf "[ %a ]"
      (exp_with Hole)
      exp_in_hole

let pp_context_exp ppf (c, e) =
  exp_with e ppf @@ fill_context (c, Hole)

let rec decompose_down (c, e as ce) =
  let ce =
    match e with
    | Var _ -> raise @@ Error (c, e)
    | IConst _
    | BConst _ -> raise Value
    | BinOp (r, op, e1, e2) -> begin
        try decompose_down (CBinOpL (r, op, c, e2), e1)
        with Value ->
        try decompose_down (CBinOpR (r, op, e1, c), e2)
        with Value -> ce
      end
    | FunExp _ -> raise Value
    | AppExp (r, e1, e2) -> begin
        try decompose_down (CAppL (r, c, e2), e1)
        with Value ->
        try decompose_down (CAppR (r, e1, c), e2)
        with Value -> ce
      end
    | CastExp (_, v, g, TyDyn) when is_value v && is_ground g -> raise Value
    | CastExp (_, v, TyFun _, TyFun _) when is_value v -> raise Value
    | CastExp (r, e1, u1, u2) -> begin
        try decompose_down (CCast (r, c, u1, u2), e1)
        with Value -> ce
      end
    | LetExp _ -> raise Value
    | Hole -> raise Decomposition_not_found
  in
  ce

let rec decompose_up (c, v) =
  if is_value v then
    match c with
    | CTop -> raise Decomposition_not_found
    | CAppL (r, c', e) -> begin
        try decompose_down (CAppR (r, v, c'), e)
        with Value -> c', AppExp (r, v, e)
      end
    | CAppR (r, v', c') -> c', AppExp (r, v', v)
    | CBinOpL (r, op, c', e) -> begin
        try decompose_down (CBinOpR (r, op, v, c'), e)
        with Value -> c', BinOp (r, op, v, e)
      end
    | CBinOpR (r, op, v', c') -> c', BinOp (r, op, v', v)
    | CCast (r, c', u1, u2) ->
      decompose_up (c', CastExp (r, v, u1, u2))
  else
    c, v

let decompose ce =
  try decompose_down ce
  with Value -> decompose_up ce

let reduce_in (c, e) =
  let e, s = reduce e in
  c, e, s

let eval_step (ce: context * exp): (context * exp) * substitution option =
  let c, e, s = reduce_in @@ decompose ce in
  match s with
  | Some s -> (subst_tv_context [s] c, subst_exp [s] e), Some s
  | None -> (c, e), None

let rec eval_all ?(debug=false) (ce: context * exp) (ss: substitutions): (context * exp) * substitutions =
  try
    if debug then fprintf std_formatter "eval_step --> %a\n" pp_context_exp ce;
    let ce, s = eval_step ce in
    let ss = begin
      match s with
      | None -> ss
      | Some s -> s :: ss
    end in
    eval_all ce ss ~debug:debug
  with Decomposition_not_found -> ce, ss

let eval ?(debug=false) env e =
  let e = Environment.fold (fun x (xs, v) f -> subst_var x xs v f) env e in
  let e, s = eval_all (CTop, e) [] ~debug:debug in
  fill_context e, List.rev s

let eval_program ?(debug=false) env p =
  match p with
  | Exp f ->
    let v, s = eval env f ~debug:debug in
    env, "-", v, s
  | LetDecl (x, xs, f) ->
    let v, s =  eval env f ~debug:debug in
    let env = Environment.add x (xs, v) env in
    env, x, v, s
