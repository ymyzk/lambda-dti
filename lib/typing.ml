open Format

open Pp
open Syntax

exception Type_error of string
(* Bug in this implementation *)
exception Type_bug of string

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyVar (!counter, ref None)
  in body

(* These functions only can be used for normalized types *)
let dom = function
  | TyVar (_, { contents = Some _ }) ->
    raise @@ Type_bug "dom: instantiated tyvar is given"
  | TyFun (u1, _) -> u1
  | TyDyn -> TyDyn
  | _ as u ->
    raise @@ Type_error (asprintf "failed to match: dom(%a)" pp_ty u)

let cod = function
  | TyVar (_, { contents = Some _ }) ->
    raise @@ Type_bug "cod: instantiated tyvar is given"
  | TyFun (_, u2) -> u2
  | TyDyn -> TyDyn
  | _ as u ->
    raise @@ Type_error (asprintf "failed to match: cod(%a)" pp_ty u)

let rec meet u1 u2 = match u1, u2 with
  | TyVar (_, { contents = Some _ }), _
  | _, TyVar (_, { contents = Some _ }) ->
    raise @@ Type_bug "meet: instantiated tyvar is given"
  | TyBool, TyBool -> TyBool
  | TyInt, TyInt -> TyInt
  | TyUnit, TyUnit -> TyUnit
  | TyVar (a1, _ as x1), TyVar (a2, _) when a1 = a2 -> TyVar x1
  | TyDyn, u | u, TyDyn -> u
  | TyFun (u11, u12), TyFun (u21, u22) -> TyFun (meet u11 u21, meet u12 u22)
  | _ ->
    raise @@ Type_error (asprintf "failed to match: meet(%a, %a)" pp_ty u1 pp_ty u2)

let type_of_binop = function
  | Plus | Minus | Mult | Div | Mod -> TyInt, TyInt, TyInt
  | Eq | Neq | Lt | Lte | Gt | Gte -> TyInt, TyInt, TyBool

let rec is_static_type = function
  | TyVar (_, { contents = Some u }) -> is_static_type u
  | TyFun (u1, u2) -> (is_static_type u1) && (is_static_type u2)
  | TyDyn -> false
  | _ -> true

let rec is_bv_type = function
  | TyBool
  | TyInt
  | TyUnit
  | TyVar (_, { contents = None }) -> true
  | TyVar (_, { contents = Some u }) -> is_bv_type u
  | _ -> false

let rec is_base_type = function
  | TyBool | TyInt | TyUnit -> true
  | TyVar (_, { contents = Some u }) -> is_base_type u
  | _ -> false

let rec is_tyvar = function
  | TyVar (_, { contents = None }) -> true
  | TyVar (_, { contents = Some u }) -> is_tyvar u
  | _ -> false

let rec is_equal u1 u2 = match u1, u2 with
  | TyVar (_, { contents = Some u1 }), u2
  | u1, TyVar (_, { contents = Some u2 }) -> is_equal u1 u2
  | TyDyn, TyDyn
  | TyBool, TyBool
  | TyInt, TyInt
  | TyUnit, TyUnit -> true
  | TyVar (a1, _), TyVar (a2, _) when a1 = a2 -> true
  | TyFun (u11, u12), TyFun (u21, u22) ->
    (is_equal u11 u21) && (is_equal u12 u22)
  | _ -> false

let rec is_consistent u1 u2 = match u1, u2 with
  | TyVar (_, { contents = Some u1 }), u2
  | u1, TyVar (_, { contents = Some u2 }) ->
    is_consistent u1 u2
  | TyDyn, TyDyn
  | TyBool, TyBool
  | TyInt, TyInt
  | TyUnit, TyUnit
  | _, TyDyn
  | TyDyn, _ -> true
  | TyVar (a1, _), TyVar (a2, _) when a1 = a2 -> true
  | TyFun (u11, u12), TyFun (u21, u22) ->
    (is_consistent u11 u21) && (is_consistent u12 u22)
  | _ -> false

(* Substitutions for type variables *)

type substitution = tyvar * ty
type substitutions = substitution list

(* S(t) *)
let subst_type (s: (tyvar * ty) list) (u: ty) =
  (* {X':->U'}(U) *)
  let rec subst u ((a', _), u' as s0) = match u with
    | TyFun (u1, u2) -> TyFun (subst u1 s0, subst u2 s0)
    | TyVar (a, { contents = None }) when a = a' -> u'
    | TyVar (_, { contents = Some u }) -> subst u s0
    | _ as u -> u
  in
  List.fold_left subst u s

(* When you're sure that this tyarg does not contain ν
 * you can convert it to ty *)
let tyarg_to_ty = function
  | CC.Ty u -> u
  | CC.TyNu -> raise @@ Type_bug "failed to cast tyarg to ty"

module ITGL = struct
  open Pp.ITGL
  open Syntax.ITGL

  (* Unification *)

  let rec unify = function
    (* When tyvar is already instantiated *)
    | CConsistent (TyVar (_, { contents = Some u1 }), u2)
    | CConsistent (u1, TyVar (_, { contents = Some u2 })) ->
      unify @@ CConsistent (u1, u2)
    (* iota ~ iota *)
    | CConsistent (u1, u2) when u1 = u2 && is_base_type u1 -> ()
    (* X ~ X *)
    | CConsistent (TyVar (a1, _), TyVar (a2, _)) when a1 = a2 -> ()
    (* ? ~ U or U ~ ? *)
    | CConsistent (TyDyn, _) | CConsistent (_, TyDyn) -> ()
    (* U11->U12 ~ U21->U22 *)
    | CConsistent (TyFun (u11, u12), TyFun (u21, u22)) ->
      unify @@ CConsistent (u11, u21);
      unify @@ CConsistent (u12, u22)
    (* U ~ X *)
    | CConsistent (u, TyVar x) when not (is_tyvar u) ->
      unify @@ CConsistent (TyVar x, u)
    (* X ~ U *)
    | CConsistent (TyVar x, u) when is_bv_type u ->
      unify @@ CEqual (TyVar x, u)
    (* X ~ U1->U2 *)
    | CConsistent (TyVar x, TyFun (u1, u2)) when not @@ TV.mem x (ftv_ty (TyFun (u1, u2))) ->
      let x1, x2 = fresh_tyvar (), fresh_tyvar () in
      unify @@ CEqual (TyVar x, TyFun (x1, x2));
      unify @@ CConsistent (x1, u1);
      unify @@ CConsistent (x2, u2)
    (* When tyvar is already instantiated *)
    | CEqual (TyVar (_, { contents = Some u1 }), u2)
    | CEqual (u1, TyVar (_, { contents = Some u2 })) ->
      unify @@ CEqual (u1, u2)
    (* CEqual can be used only for static types *)
    | CEqual (u1, u2) as c when not (is_static_type u1 && is_static_type u2) ->
      raise @@ Type_bug (asprintf "invalid constraint: %a" pp_constr c)
    (* iota = iota *)
    | CEqual (t1, t2) when t1 = t2 && is_base_type t1 -> ()
    (* X = X *)
    | CEqual (TyVar (a1, _), TyVar (a2, _)) when a1 = a2 -> ()
    (* T11->T12 = T21->T22 *)
    | CEqual (TyFun (t11, t12), TyFun (t21, t22)) ->
      unify @@ CEqual (t11, t21);
      unify @@ CEqual (t12, t22)
    (* T = X *)
    | CEqual (t, TyVar x) when not (is_tyvar t) ->
      unify @@ CEqual (TyVar x, t)
    (* X = T *)
    | CEqual (TyVar (a, x), t) when not (TV.mem (a, x) (ftv_ty t)) ->
      x := Some t
    | _ as c ->
      raise @@ Type_error (asprintf "cannot solve a constraint: %a" pp_constr c)

  (* Utility for type inference *)

  let rec type_of_cod_eq = function
    | TyVar (_, { contents = Some u }) -> type_of_cod_eq u
    | TyVar (_, { contents = None }) as u ->
      let x1, x2 = fresh_tyvar (), fresh_tyvar () in
      unify @@ CEqual (u, (TyFun (x1, x2)));
      x2
    | TyFun (_, u2) -> u2
    | TyDyn -> TyDyn
    | _ as u ->
      raise @@ Type_error (
        asprintf "failed to generate constraints: cod(%a)" pp_ty u
      )

  let rec type_of_dom_con u1 u2 = match u1, u2 with
    | TyVar (_, { contents = Some u1 }), u2
    | u1, TyVar (_, { contents = Some u2 }) ->
      type_of_dom_con u1 u2
    | TyVar (_, { contents = None }) as u, u2 ->
      let x1, x2 = fresh_tyvar (), fresh_tyvar () in
      unify @@ CEqual (u, (TyFun (x1, x2)));
      unify @@ CConsistent (x1, u2)
    | TyFun (u11, _), u2 ->
      unify @@ CConsistent (u11, u2)
    | TyDyn, u2 ->
      unify @@ CConsistent (u1, u2)
    | u1, u2 ->
      raise @@ Type_error (
        asprintf "failed to generate constraints: dom(%a) ~ %a" pp_ty u1 pp_ty u2
      )

  let rec type_of_meet u1 u2 = match u1, u2 with
    | TyVar (_, { contents = Some u1 }), u2
    | u1, TyVar (_, { contents = Some u2 }) ->
      type_of_meet u1 u2
    | TyBool, TyBool -> TyBool
    | TyInt, TyInt -> TyInt
    | TyUnit, TyUnit -> TyUnit
    | TyDyn, u
    | u, TyDyn ->
      unify @@ CConsistent (u, TyDyn);
      u
    | TyVar x, u
    | u, TyVar x ->
      unify @@ CConsistent (u, TyVar x);
      TyVar x
    | TyFun (u11, u12), TyFun (u21, u22) ->
      let u1 = type_of_meet u11 u21 in
      let u2 = type_of_meet u12 u22 in
      TyFun (u1, u2)
    | u1, u2 -> raise @@ Type_error (
        asprintf "failed to generate constraints: meet(%a, %a)"
          pp_ty u1 pp_ty u2
      )

  (* Utility functions for let polymorpism *)

  let closure_tyvars1 u1 env v1 =
    TV.elements @@ TV.diff (ftv_ty u1) @@ TV.union (ftv_tyenv env) (ftv_exp v1)

  let closure_tyvars_let_decl e u1 env =
    TV.elements @@ TV.diff (TV.union (tv_exp e) (ftv_ty u1)) (ftv_tyenv env)

  let closure_tyvars2 w1 env u1 v1 =
    let ftvs = TV.big_union [ftv_tyenv env; ftv_ty u1; ftv_exp v1] in
    TV.elements @@ TV.diff (Syntax.CC.ftv_exp w1) ftvs

  let rec is_base_value env u =
    assert (u = TyInt || u = TyBool || u = TyUnit);
    function
    | Var (_, x, ys) ->
      begin try
        let TyScheme (xs, u') = Environment.find x env in
        let s = Utils.List.zip xs !ys in
        subst_type s u' = u
      with Not_found ->
        raise @@ Type_bug (asprintf "variable '%s' not found in the environment" x)
      end
    | IConst _ when u = TyInt -> true
    | BConst _ when u = TyBool -> true
    | UConst _ when u = TyUnit -> true
    | AscExp (r, e, TyVar (_, { contents = Some u' })) ->
      is_base_value env u @@ AscExp (r, e, u')
    | AscExp (_, e, u') when u = u' -> is_base_value env u e
    | _ -> false

  let rec is_fun_value env = function
    | Var (_, x, ys) ->
      begin try
        begin
          let TyScheme (xs, u') = Environment.find x env in
          let s = Utils.List.zip xs !ys in
          begin match subst_type s u' with
          | TyFun _ -> true
          | _ -> false
          end
        end with Not_found ->
          raise @@ Type_bug (asprintf "variable '%s' not found in the environment" x)
      end
    | FunEExp _
    | FunIExp _
    | FixEExp _
    | FixIExp _ -> true
    | AscExp (_, e, TyFun _) -> is_fun_value env e
    | AscExp (r, e, TyVar (_, { contents = Some u })) -> is_fun_value env @@ AscExp (r, e, u)
    | _ -> false

  let rec is_tyvar_value env x = function
    | Var (_, x', ys) ->
      begin
        try
          let TyScheme (xs, u') = Environment.find x' env in
          let s = Utils.List.zip xs !ys in
          begin match subst_type s u' with
          | TyVar (x'', _) when x = x'' -> true
          | _ -> false
          end
        with Not_found ->
          raise @@ Type_bug (asprintf "variable '%s' not found in the environment" x')
      end
    | AscExp (r, e, TyVar (_, { contents = Some u })) ->
        is_tyvar_value env x @@ AscExp (r, e, u)
    | AscExp (_, e, TyVar (x', { contents = None })) when x = x' ->
        is_tyvar_value env x e
    | _ -> false

  (** Returns true if a given expression is a "value" under the given environment.
   * The definition of "value" slightly differs that in the paper
   * to allow more type variables are generalized by let. *)
  let rec is_value env = function
    | Var _
    | IConst _
    | BConst _
    | UConst _
    | FunEExp _
    | FunIExp _
    | FixEExp _
    | FixIExp _ -> true
    | AscExp (_, e, (TyInt | TyBool | TyUnit as u)) when is_base_value env u e -> true
    | AscExp (_, e, TyFun _) when is_fun_value env e -> true
    | AscExp (_, e, TyDyn) when is_value env e -> true
    | AscExp (r, e, TyVar (_, { contents = Some u })) -> is_value env @@ AscExp (r, e, u)
    | AscExp (_, e, TyVar (x, { contents = None })) -> is_tyvar_value env x e
    | _ -> false

  (* Type inference *)

  let rec type_of_exp env = function
    | Var (_, x, ys) ->
      begin
        try
          let TyScheme (xs, u) = Environment.find x env in
          (* Replace type variables with fresh ones *)
          ys := List.map (fun _ -> fresh_tyvar ()) xs;
          let s = Utils.List.zip xs !ys in
          subst_type s u
        with Not_found ->
          raise @@ Type_error (asprintf "variable '%s' not found in the environment" x)
      end
    | IConst _ -> TyInt
    | BConst _ -> TyBool
    | UConst _ -> TyUnit
    | BinOp (_, op, e1, e2) ->
      let ui1, ui2, ui = type_of_binop op in
      let u1 = type_of_exp env e1 in
      let u2 = type_of_exp env e2 in
      unify @@ CConsistent (u1, ui1);
      unify @@ CConsistent (u2, ui2);
      ui
    | AscExp (_, e, u1) ->
      let u = type_of_exp env e in
      unify @@ CConsistent (u, u1);
      u1
    | IfExp (_, e1, e2, e3) ->
      let u1 = type_of_exp env e1 in
      let u2 = type_of_exp env e2 in
      let u3 = type_of_exp env e3 in
      unify @@ CConsistent (u1, TyBool);
      type_of_meet u2 u3
    | FunEExp (_, x, u1, e)
    | FunIExp (_, x, u1, e) ->
      let u2 = type_of_exp (Environment.add x (tysc_of_ty u1) env) e in
      TyFun (u1, u2)
    | FixEExp (_, x, y, u1, u2, e)
    | FixIExp (_, x, y, u1, u2, e) ->
      let env = Environment.add x (tysc_of_ty (TyFun (u1, u2))) env in
      let env = Environment.add y (tysc_of_ty u1) env in
      let u2' = type_of_exp env e in
      unify @@ CConsistent (u2, u2');
      TyFun (u1, u2)
    | AppExp (_, e1, e2) ->
      let u1 = type_of_exp env e1 in
      let u2 = type_of_exp env e2 in
      let u3 = type_of_cod_eq u1 in
      type_of_dom_con u1 u2;
      u3
    | LetExp (r, x, e1, e2) ->
      let u1 = type_of_exp env e1 in
      if is_value env e1 then
        let xs = closure_tyvars1 u1 env e1 in
        let us1 = TyScheme (xs, u1) in
        type_of_exp (Environment.add x us1 env) e2
      else
        type_of_exp env @@ AppExp (r, FunIExp (r, x, u1, e2), e1)

  let type_of_program env = function
    | Exp e ->
      Exp e, type_of_exp env e
    | LetDecl (x, e) ->
      let u = type_of_exp env e in
      LetDecl (x, e), u

  (* Normalize type variables *)

  let rec normalize_type = function
    | TyVar (_, { contents = Some u }) -> normalize_type u
    | TyFun (u1, u2) -> TyFun (normalize_type u1, normalize_type u2)
    | _ as u -> u

  let normalize_tyenv =
    Environment.map @@ fun (TyScheme (xs, u)) -> TyScheme (xs, normalize_type u)

  let rec normalize_exp = function
    | Var (r, x, ys) -> Var (r, x, ref @@ List.map normalize_type !ys)
    | IConst _
    | BConst _
    | UConst _ as e -> e
    | BinOp (r, op, e1, e2) ->
      BinOp (r, op, normalize_exp e1, normalize_exp e2)
    | AscExp (r, e, u) ->
      AscExp (r, normalize_exp e, normalize_type u)
    | IfExp (r, e1, e2, e3) ->
      IfExp (r, normalize_exp e1, normalize_exp e2, normalize_exp e3)
    | FunEExp (r, x1, u1, e) ->
      FunEExp (r, x1, normalize_type u1, normalize_exp e)
    | FunIExp (r, x1, u1, e) ->
      FunIExp (r, x1, normalize_type u1, normalize_exp e)
    | FixEExp (r, x, y, u1, u2, e) ->
      FixEExp (r, x, y, normalize_type u1, normalize_type u2, normalize_exp e)
    | FixIExp (r, x, y, u1, u2, e) ->
      FixIExp (r, x, y, normalize_type u1, normalize_type u2, normalize_exp e)
    | AppExp (r, e1, e2) ->
      AppExp (r, normalize_exp e1, normalize_exp e2)
    | LetExp (r, y, e1, e2) ->
      LetExp (r, y, normalize_exp e1, normalize_exp e2)

  let normalize_program = function
    | Exp e -> Exp (normalize_exp e)
    | LetDecl (x, e) -> LetDecl (x, normalize_exp e)

  let normalize env p u =
    normalize_tyenv env,
    normalize_program p,
    normalize_type u

  (* Cast insertion translation *)

  let cast f u1 u2 =
    if u1 = u2 then f  (* Omit identity cast for better performance *)
    else CC.CastExp (CC.range_of_exp f, f, u1, u2, Pos)

  let rec translate_exp env = function
    | Var (r, x, ys) -> begin
        try
          let TyScheme (xs, u) = Environment.find x env in
          let ftvs = ftv_ty u in
          let s = Utils.List.zip xs !ys in
          let ys = List.map (fun (x, u) -> if TV.mem x ftvs then CC.Ty u else CC.TyNu) s
          in
          let ys = ys @ Utils.List.repeat CC.TyNu (List.length xs - List.length ys) in
          let u = subst_type (List.filter (fun (x, _) -> TV.mem x ftvs) s) u in
          CC.Var (r, x, ys), u
        with Not_found ->
          raise @@ Type_bug "variable not found during cast-inserting translation"
      end
    | IConst (r, i) -> CC.IConst (r, i), TyInt
    | BConst (r, b) -> CC.BConst (r, b), TyBool
    | UConst r -> CC.UConst r, TyUnit
    | BinOp (r, op, e1, e2) ->
      let ui1, ui2, ui = type_of_binop op in
      let f1, u1 = translate_exp env e1 in
      let f2, u2 = translate_exp env e2 in
      CC.BinOp (r, op, cast f1 u1 ui1, cast f2 u2 ui2), ui
    | AscExp (_, e, u1) ->
      let f, u = translate_exp env e in
      if is_consistent u u1 then
        cast f u u1, u1
      else
        raise @@ Type_bug "type ascription"
    | IfExp (r, e1, e2, e3) ->
      let f1, u1 = translate_exp env e1 in
      let f2, u2 = translate_exp env e2 in
      let f3, u3 = translate_exp env e3 in
      let u = meet u2 u3 in
      CC.IfExp (r, cast f1 u1 TyBool, cast f2 u2 u, cast f3 u3 u), u
    | FunEExp (r, x, u1, e)
    | FunIExp (r, x, u1, e) ->
      let f, u2 = translate_exp (Environment.add x (tysc_of_ty u1) env) e in
      CC.FunExp (r, x, u1, f), TyFun (u1, u2)
    | FixEExp (r, x, y, u1, u2, e)
    | FixIExp (r, x, y, u1, u2, e) ->
      (* NOTE: Disallow to use x polymorphically in e *)
      let env = Environment.add x (tysc_of_ty (TyFun (u1, u2))) env in
      let env = Environment.add y (tysc_of_ty u1) env in
      let f, u2' = translate_exp env e in
      CC.FixExp (r, x, y, u1, u2, cast f u2' u2), TyFun (u1, u2)
    | AppExp (r, e1, e2) ->
      let f1, u1 = translate_exp env e1 in
      let f2, u2 = translate_exp env e2 in
      CC.AppExp (r, cast f1 u1 (TyFun (dom u1, cod u1)), cast f2 u2 (dom u1)), cod u1
    | LetExp (r, x, e1, e2) when is_value env e1 ->
      let f1, u1 = translate_exp env e1 in
      let xs = closure_tyvars1 u1 env e1 in
      let ys = closure_tyvars2 f1 env u1 e1 in
      let xys = xs @ ys in
      let us1 = TyScheme (xys, u1) in
      let f2, u2 = translate_exp (Environment.add x us1 env) e2 in
      CC.LetExp (r, x, xys, f1, f2), u2
    | LetExp (r, x, e1, e2) ->
      let _, u1 = translate_exp env e1 in
      let e = AppExp (r, FunIExp (r, x, u1, e2), e1) in
      translate_exp env e

  let translate env = function
    | Exp e ->
      let f, u = translate_exp env e in
      env, CC.Exp f, u
    | LetDecl (x, e) when is_value env e ->
      let f, u = translate_exp env e in
      let xs = closure_tyvars_let_decl e u env in
      let ys = closure_tyvars2 f env u e in
      let env = Environment.add x (TyScheme (xs @ ys, u)) env in
      env, CC.LetDecl (x, xs @ ys, f), u
    | LetDecl (x, e) ->
      let f, u = translate_exp env e in
      let env = Environment.add x (tysc_of_ty u) env in
      env, CC.LetDecl (x, [], f), u
end

module CC = struct
  open Syntax.CC

  let rec type_of_exp env = function
    | Var (_, x, ys) -> begin
        try
          let TyScheme (xs, u) = Environment.find x env in
          if List.length xs = List.length ys then
            let ftvs = ftv_ty u in
            let s = Utils.List.zip xs ys in
            let s = List.filter (fun (x, _) -> TV.mem x ftvs) s in
            let s = List.map (fun (x, u) -> x, tyarg_to_ty u) s in
            subst_type s u
          else
            raise @@ Type_bug "invalid type application"
        with Not_found ->
          raise @@ Type_bug "variable not found"
      end
    | IConst _ -> TyInt
    | BConst _ -> TyBool
    | UConst _ -> TyUnit
    | BinOp (_, op, f1, f2) ->
      let u1 = type_of_exp env f1 in
      let u2 = type_of_exp env f2 in
      let ui1, ui2, ui = type_of_binop op in
      if (u1, u2) = (ui1, ui2) then
        ui
      else
        raise @@ Type_bug "binop"
    | IfExp (_, f1, f2, f3) ->
      let u1 = type_of_exp env f1 in
      let u2 = type_of_exp env f2 in
      let u3 = type_of_exp env f3 in
      if u1 = TyBool && u2 = u3 then
        u2
      else
        raise @@ Type_bug "if"
    | FunExp (_, x, u1, f) ->
      let u2 = type_of_exp (Environment.add x (tysc_of_ty u1) env) f in
      TyFun (u1, u2)
    | FixExp (_, x, y, u1, u, f) ->
      let u2 = type_of_exp (Environment.add y (tysc_of_ty u1) (Environment.add x (tysc_of_ty (TyFun (u1, u))) env)) f in
      TyFun (u1, u2)
    | AppExp (_, f1, f2) ->
      let u1 = type_of_exp env f1 in
      let u2 = type_of_exp env f2 in
      begin match u1, u2 with
        | TyFun (u11, u12), u2 when u11 = u2 ->
          u12
        | _ -> raise @@ Type_bug "app"
      end
    | CastExp (r, f, TyVar (_, { contents = Some u1 }), u2, p)
    | CastExp (r, f, u1, TyVar (_, { contents = Some u2 }), p) ->
      type_of_exp env @@ CastExp (r, f, u1, u2, p)
    | CastExp (_, f, u1, u2, _) ->
      let u = type_of_exp env f in
      if u = u1 then
        if is_consistent u1 u2 then
          u2
        else
          raise @@ Type_bug "not consistent"
      else
        raise @@ Type_bug "invalid source type"
    | LetExp (_, x, xs, f1, f2) when is_value f1 ->
      let u1 = type_of_exp env f1 in
      let us1 = TyScheme (xs, u1) in
      let u2 = type_of_exp (Environment.add x us1 env) f2 in
      u2
    | LetExp _ ->
      raise @@ Type_bug "invalid translation for let expression"

  let type_of_program env = function
    | Exp e -> type_of_exp env e
    | LetDecl (_, _, f) -> type_of_exp env f
end
