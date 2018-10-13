open Format
open Syntax
open Syntax.CC
open Utils.Error

exception Blame of range * Syntax.CC.polarity

exception Eval_bug of string

let subst_type = Typing.subst_type

let rec subst_exp s = function
  | Var (r, x, ys) ->
    let subst_type = function
      | Ty u -> Ty (subst_type s u)
      | TyNu -> TyNu
    in
    Var (r, x, List.map subst_type ys)
  | IConst _
  | BConst _
  | UConst _ as f -> f
  | BinOp (r, op, f1, f2) -> BinOp (r, op, subst_exp s f1, subst_exp s f2)
  | IfExp (r, f1, f2, f3) -> IfExp (r, subst_exp s f1, subst_exp s f2, subst_exp s f3)
  | FunExp (r, x1, u1, f) -> FunExp (r, x1, subst_type s u1, subst_exp s f)
  | FixExp (r, x, y, u1, u2, f) ->
    FixExp (r, x, y, subst_type s u1, subst_type s u2, subst_exp s f)
  | AppExp (r, f1, f2) -> AppExp (r, subst_exp s f1, subst_exp s f2)
  | CastExp (r, f, u1, u2, p) -> CastExp (r, subst_exp s f, subst_type s u1, subst_type s u2, p)
  | LetExp (r, y, ys, f1, f2) ->
    (* Remove substitutions captured by let exp s *)
    let s = List.filter (fun (x, _) -> not @@ List.memq x ys) s in
    LetExp (r, y, ys, subst_exp s f1, subst_exp s f2)

let eval_binop op v1 v2 =
  begin match op, v1, v2 with
    | Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
    | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
    | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
    | Div, IntV i1, IntV i2 -> IntV (i1 / i2)
    | Mod, IntV i1, IntV i2 -> IntV (i1 mod i2)
    | Eq, IntV i1, IntV i2 -> BoolV (i1 = i2)
    | Neq, IntV i1, IntV i2 -> BoolV (i1 <> i2)
    | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
    | Lte, IntV i1, IntV i2 -> BoolV (i1 <= i2)
    | Gt, IntV i1, IntV i2 -> BoolV (i1 > i2)
    | Gte, IntV i1, IntV i2 -> BoolV (i1 >= i2)
    | _ -> raise @@ Eval_bug "binop: unexpected type of argument"
  end

let nu_to_fresh = function
  | Ty u -> u
  | TyNu -> Typing.fresh_tyvar ()

let rec eval ?(debug=false) (env: (tyvar list * value) Environment.t) f =
  if debug then fprintf err_formatter "eval <-- %a\n" Pp.CC.pp_exp f;
  let eval = eval ~debug:debug in
  match f with
  | Var (_, x, us) ->
    let xs, v = Environment.find x env in
    let us = List.map nu_to_fresh us in
    begin match v with
      | FunV proc -> FunV (fun _ -> proc (xs, us))
      | _ -> v
    end
  | IConst (_, i) -> IntV i
  | BConst (_, b) -> BoolV b
  | UConst _ -> UnitV
  | BinOp (_, op, f1, f2) ->
    let v1 = eval env f1 in
    let v2 = eval env f2 in
    eval_binop op v1 v2
  | FunExp (_, x, _, f') ->
    FunV (
      fun (xs, ys) -> fun v ->
        eval (Environment.add x ([], v) env) @@ subst_exp (Utils.List.zip xs ys) f'
    )
  | FixExp (_, x, y, _, _, f') ->
    let f (xs, ys) v =
      let f' = subst_exp (Utils.List.zip xs ys) f' in
      let rec f _ v =
        let env = Environment.add x (xs, FunV f) env in
        let env = Environment.add y ([], v) env in
        eval env f'
      in f ([], []) v
    in FunV f
  | AppExp (_, f1, f2) ->
    let v1 = eval env f1 in
    let v2 = eval env f2 in
    begin match v1 with
      | FunV proc -> proc ([], []) v2
      | _ -> raise @@ Eval_bug "app: application of non procedure value"
    end
  | IfExp (_, f1, f2, f3) ->
    let v1 = eval env f1 in
    begin match v1 with
      | BoolV true -> eval env f2
      | BoolV false -> eval env f3
      | _ -> raise @@ Eval_bug "if: non boolean value"
    end
  | LetExp (_, x, xs, f1, f2) ->
    let v1 = eval env f1 in
    eval (Environment.add x (xs, v1) env) f2
  | CastExp (r, f, u1, u2, p) ->
    let v = eval env f in
    cast ~debug:debug v u1 u2 r p
and cast ?(debug=false) v u1 u2 r p =
  let print_debug f =
    if debug then
      fprintf err_formatter f
    else
      fprintf Utils.Format.empty_formatter f
  in
  print_debug "cast <-- %a: %a => %a\n" Pp.CC.pp_value v Pp.pp_ty u1 Pp.pp_ty u2;
  let cast = cast ~debug:debug in
  match u1, u2 with
  (* When type variables are instantiated *)
  | TyVar (_, { contents = Some u1 }), u2
  | u1, TyVar (_, { contents = Some u2 }) ->
    cast v u1 u2 r p
  (* IdBase *)
  | TyBool, TyBool
  | TyInt, TyInt
  | TyUnit, TyUnit -> v
  (* IdStar *)
  | TyDyn, TyDyn -> v
  (* Succeed / Fail *)
  | TyDyn, (TyBool | TyInt | TyUnit | TyFun (TyDyn, TyDyn) as u2) -> begin
      match v, u2 with
      | Tagged (B, v), TyBool -> v
      | Tagged (I, v), TyInt -> v
      | Tagged (U, v), TyUnit -> v
      | Tagged (Ar, v), TyFun (TyDyn, TyDyn) -> v
      | Tagged _, _ -> raise @@ Blame (r, p)
      | _ -> raise @@ Eval_bug "untagged value"
    end
  (* AppCast *)
  | TyFun (u11, u12), TyFun (u21, u22) -> begin
      match v with
      | FunV proc ->
        FunV (
          fun (xs, ys) x ->
            let subst = subst_type @@ Utils.List.zip xs ys in
            let arg = cast x (subst u21) (subst u11) r @@ neg p in
            let res = proc (xs, ys) arg in
            cast res (subst u12) (subst u22) r p
        )
      | _ -> raise @@ Eval_bug "non procedural value"
    end
  (* Tagged *)
  | TyBool, TyDyn -> Tagged (B, v)
  | TyInt, TyDyn -> Tagged (I, v)
  | TyUnit, TyDyn -> Tagged (U, v)
  | TyFun (TyDyn, TyDyn), TyDyn -> Tagged (Ar, v)
  (* Ground *)
  | TyFun _, TyDyn ->
    let dfun = TyFun (TyDyn, TyDyn) in
    let v = cast v u1 dfun r p in
    cast v dfun TyDyn r p
  (* Expand *)
  | TyDyn, TyFun _ ->
    let dfun = TyFun (TyDyn, TyDyn) in
    let v = cast v TyDyn dfun r p in
    cast v dfun u2 r p
  (* InstBase / InstArrow *)
  | TyDyn, (TyVar (_, ({ contents = None } as x)) as x') -> begin
    match v with
      | Tagged (B | I | U as t, v) ->
        let u = tag_to_ty t in
        print_debug "DTI: %a is instantiated to %a\n"
          Pp.pp_ty x'
          Pp.pp_ty u;
        x := Some u;
        v
      | Tagged (Ar, v) ->
        let u = TyFun (Typing.fresh_tyvar (), Typing.fresh_tyvar ()) in
        print_debug "DTI: %a is instantiated to %a\n"
          Pp.pp_ty x'
          Pp.pp_ty u;
        x := Some u;
        cast v (TyFun (TyDyn, TyDyn)) u r p
      | _ -> raise @@ Eval_bug "cannot instantiate"
    end
  | _ -> raise @@ Eval_bug (asprintf "cannot cast value: %a" Pp.CC.pp_value v)

let eval_program ?(debug=false) env p =
  match p with
  | Exp f ->
    let v = eval env f ~debug:debug in
    env, "-", v
  | LetDecl (x, xs, f) ->
    let v = eval env f ~debug:debug in
    let env = Environment.add x (xs, v) env in
    env, x, v
