open Format
open Syntax
open Syntax.CC
open Utils.Error

exception Blame of range
exception Reduce

(* Gradual Type Parameters Substitution *)

type substitution = typaram * ty
type substitutions = substitution list

(* s(u) *)
let subst_gtp_in_type s u =
  let rec subst_gtp (a, u as s) = function
    | TyFun (u1, u2) -> TyFun (subst_gtp s u1, subst_gtp s u2)
    | TyGParam a' when a = a' -> u
    | _ as u -> u
  in
  List.fold_left (fun u -> fun s0 -> subst_gtp s0 u) u s

(* s(e) *)
let rec subst_gtp_in_exp s e =
  map_exp (fun u -> subst_gtp_in_type s u) (subst_gtp_in_exp s) e

(* s(c) *)
let rec subst_gtp_in_context s = function
  | CTop -> CTop
  | CAppL (r, c, e) -> CAppL (r, subst_gtp_in_context s c, subst_gtp_in_exp s e)
  | CAppR (r, e, c) -> CAppR (r, subst_gtp_in_exp s e, subst_gtp_in_context s c)
  | CBinOpL (r, op, c, e) -> CBinOpL (r, op, subst_gtp_in_context s c, subst_gtp_in_exp s e)
  | CBinOpR (r, op, e, c) -> CBinOpR (r, op, subst_gtp_in_exp s e, subst_gtp_in_context s c)
  | CCast (r, c, u1, u2) -> CCast (r, subst_gtp_in_context s c, subst_gtp_in_type s u1, subst_gtp_in_type s u2)

let pp_sep ppf () = fprintf ppf ", "

let pp_substitution ppf (a, u) =
  fprintf ppf "['a%d :-> %a]" a Pp.pp_ty u

let pp_substitutions ppf ss =
  pp_print_list pp_substitution ppf ss ~pp_sep:pp_sep

(* Reduction *)

(* TODO: better name *)
let subst_tyvar_tyapp (e: exp) (s: (tyvar * ty) list): exp =
  let rec subst_exp (x: tyvar) (t: ty) (e: exp): exp =
    let subst_exp = subst_exp x t in
    let subst_type = Typing.subst_type x t in
    match e with
    | Var (r, x, ys) -> Var (r, x, List.map subst_type ys)
    | IConst _
    | BConst _ as e -> e
    | BinOp (r, op, e1, e2) -> BinOp (r, op, subst_exp e1, subst_exp e2)
    | FunExp (r, x1, u1, f) -> FunExp (r, x1, subst_type u1, subst_exp f)
    | AppExp (r, e1, e2) -> AppExp (r, subst_exp e1, subst_exp e2)
    | CastExp (r, e, u1, u2) -> CastExp (r, subst_exp e, subst_type u1, subst_type u2)
    | LetExp (r, y, ys, e1, e2) ->
      LetExp (r, y, ys, (if List.mem x ys then e1 else subst_exp e1), subst_exp e2)
  in
  List.fold_left (fun e (x, y) -> subst_exp x y e) e s

(* e[x:=v] *)
let rec subst_var (x: id) (xs: tyvar list) (v: exp) (e: exp): exp =
  assert (is_value v);
  let subst = subst_var x xs v in
  match e with
  | Var (_, y, ys) ->
    if x <> y then
      e
    else
      subst_tyvar_tyapp v @@ Utils.zip xs ys
  | IConst _
  | BConst _ -> e
  | BinOp (r, op, e1, e2) -> BinOp (r, op, subst e1, subst e2)
  | FunExp (r, y, u, e') ->
    if x = y then e else FunExp (r, y, u, subst e')
  | AppExp (r, e1, e2) -> AppExp (r, subst e1, subst e2)
  | CastExp (r, e1, u1, u2) -> CastExp (r, subst e1, u1, u2)
  | LetExp (r, y, ys, f1, f2) ->
      LetExp (r, y, ys, (if x = y then f1 else subst f1), subst f2)

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
  (* R_Ground *)
  | CastExp (r, v, u, TyDyn) when u <> TyDyn && u <> ground_of_ty u ->
    let g = ground_of_ty u in
    CastExp (r, CastExp(r, v, u, g), g, TyDyn), None
  (* R_Expand *)
  | CastExp (r, v, TyDyn, u) when u <> TyDyn && u <> ground_of_ty u ->
    let g = ground_of_ty u in
    CastExp (r, CastExp(r, v, TyDyn, g), g, u), None
  | CastExp (_, CastExp (r2, v, u1, u2), u2', u3) when is_value v && u2 = u2' ->
    begin match (u1, u2, u3) with
      (* R_Succeed *)
      | g1, TyDyn, g2 when is_ground g1 && is_ground g2 && g1 = g2 -> v, None
      (* R_InstBase *)
      | TyBool, TyDyn, TyGParam a -> v, Some (a, TyBool)
      | TyInt, TyDyn, TyGParam a -> v, Some (a, TyInt)
      (* R_InstArrow *)
      | TyFun (TyDyn, TyDyn), TyDyn, TyGParam a ->
        let a1, a2 = Typing.fresh_gparam (), Typing.fresh_gparam () in
        CastExp (r2, v, TyFun (TyDyn, TyDyn), TyFun (a1, a2)), Some (a, TyFun (a1, a2))
      (* R_Fail *)
      | g1, TyDyn, g2 when is_ground g1 && is_ground g2 && g1 <> g2 ->
        raise @@ Blame (r2)
      | _ -> raise Reduce
    end
  (* R_LetP *)
  | LetExp (_, x, xs, v1, f2) when is_value v1 ->
    subst_var x xs v1 f2, None
  | _ -> raise Reduce

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

let hole = IConst (dummy_range, -12345)
let rec exp_with exp_in_hole ppf =
  let exp = exp_with exp_in_hole in
  function
  | IConst _ as c when c = hole ->
    fprintf ppf "[ %a ]"
      (exp_with hole)
      exp_in_hole
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

let pp_context_exp ppf (c, e) =
  exp_with e ppf @@ fill_context (c, hole)

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
  in
  ce

let rec decompose_up (c, v) =
  if is_value v then
    match c with
    | CTop -> raise Not_found
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
  | Some s -> (subst_gtp_in_context [s] c, subst_gtp_in_exp [s] e), Some s
  | None -> (c, e), None

let rec eval_all ?(debug=false) (ce: context * exp) (ss: substitutions): (context * exp) * substitutions =
  try
    let ce, s = eval_step ce in
    if debug then fprintf std_formatter "eval_step --> %a\n" pp_context_exp ce;
    let ss = begin
      match s with
      | None -> ss
      | Some s -> s :: ss
    end in
    eval_all ce ss ~debug:debug
  with Not_found -> ce, ss

let eval ?(debug=false) (e: exp): exp * substitutions =
  let e, s = eval_all (CTop, e) [] ~debug:debug in
  fill_context e, List.rev s
