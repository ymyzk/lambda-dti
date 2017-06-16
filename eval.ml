open Format
open Syntax
open Syntax.CC

exception Eval_error of string

type substitution = typaram * ty
type substitutions = substitution list

(* [x:=t]u *)
let rec substitute_type a u = function
  | TyFun (u1, u2) -> TyFun (substitute_type a u u1, substitute_type a u u2)
  | TyGParam a' when a = a' -> u
  | _ as u -> u

let substitute_type_with_substitutions t s =
  List.fold_left (fun u -> fun (x, t) -> substitute_type x t u) t s

let rec eval term env subst = match term with
  | Var x ->
      if Environment.mem x env then
        Environment.find x env, subst
      else
        raise @@ Eval_error "variable not found"
  | IConst i -> IntV i, subst
  | BConst b -> BoolV b, subst
  | BinOp (op, e1, e2) ->
      let v1, subst = eval e1 env subst in
      let v2, subst = eval e2 env subst in
      begin
        match op, v1, v2 with
          | Plus, IntV x1, IntV x2 -> IntV (x1 + x2), subst
          | Mult, IntV x1, IntV x2 -> IntV (x1 * x2), subst
          | Lt, IntV x1, IntV x2 -> BoolV (x1 < x2), subst
          | _ -> raise @@ Eval_error "binop"
      end
  | FunExp (x, _, e) ->
      (* TODO: subst *)
      FunV (fun v -> fst (eval e (Environment.add x v env) subst)), subst
  | AppExp (e1, e2) ->
      let v1, subst = eval e1 env subst in
      let v2, subst = eval e2 env subst in
      begin
        match v1 with
          | FunV f -> f v2, subst
          | _ -> raise @@ Eval_error "non procedure"
      end
  | CastExp (e, t1, t2) ->
      let v, subst = eval e env subst in
      let v, subst = cast subst v t1 t2 in
      v, subst
and cast subst v t1 t2 =
  let t1 = substitute_type_with_substitutions t1 subst in
  let t2 = substitute_type_with_substitutions t2 subst in
  match t1, t2 with
  (* IdBase *)
  | TyBool, TyBool -> v, subst
  | TyInt, TyInt -> v, subst
  (* IdStar *)
  | TyDyn, TyDyn -> v, subst
  (* Succeed / Fail *)
  | TyDyn, TyBool -> begin
      match v with
      | Tagged (B, v) -> v, subst
      | Tagged (G a, v) -> v, (a, TyBool) :: subst
      | _ -> raise @@ Eval_error "blame: tagged value is not type of bool"
      end
  | TyDyn, TyInt -> begin
      match v with
      | Tagged (I, v) -> v, subst
      | Tagged (G a, v) -> v, (a, TyInt) :: subst
      | _ -> raise @@ Eval_error "blame: tagged value is not type of int"
    end
  | TyDyn, TyGParam a1 -> begin
      match v with
      | Tagged (G a2, v) when a1 = a2 -> v, subst
      | Tagged (G a2, v) when a1 <> a2 -> v, (a1, TyGParam a2) :: subst
      | Tagged (B, v) -> v, (a1, TyBool) :: subst
      | Tagged (I, v) -> v, (a1, TyInt) :: subst
      | Tagged (Ar, v) ->
          let a11, a12 = Typing.fresh_gparam (), Typing.fresh_gparam () in
          v, (a1, TyFun (a11, a12)) :: subst
      | _ -> raise @@ Eval_error "blame: tagged value is not type of gradual parameter"
    end
  | TyDyn, TyFun (TyDyn, TyDyn) -> begin
      match v with
      | Tagged (Ar, v) -> v, subst
      | Tagged (G a, v) ->
          let a1, a2 = Typing.fresh_gparam (), Typing.fresh_gparam () in
          v, (a, TyFun (a1, a2)) :: subst
      | _ -> raise @@ Eval_error "blame: tagged value is not type of fun"
    end
  (* AppCast *)
  | TyFun (u11, u12), TyFun (u21, u22) -> begin
      match v with
      | FunV f ->
          FunV (
            (* TODO *)
            fun x ->
              let arg, subst = cast subst x u21 u11 in
              let proc, _ = cast subst (f arg) u12 u22 in
              proc
          ), subst
      | _ -> raise @@ Eval_error "failed to wrap"
    end
  (* Ground *)
  | TyBool, TyDyn -> Tagged (B, v), subst
  | TyInt, TyDyn -> Tagged (I, v), subst
  | TyGParam a, TyDyn -> Tagged (G a, v), subst
  | TyFun (TyDyn, TyDyn), TyDyn -> Tagged (Ar, v), subst
  | TyFun (u11, u12), TyDyn when u11 <> TyDyn && u12 <> TyDyn ->
      let v, subst = cast subst v (TyFun (u11, u12)) (TyFun (TyDyn, TyDyn)) in
      Tagged (Ar, v), subst
  (* Expand *)
  | TyDyn, TyFun (u11, u12) when u11 <> TyDyn && u12 <> TyDyn ->
      let arg, subst = cast subst v TyDyn (TyFun (TyDyn, TyDyn)) in
      cast subst
        arg
        (TyFun (TyDyn, TyDyn))
        (TyFun (u11, u12))
  | _ ->
      raise @@ Eval_error "failed to cast"
