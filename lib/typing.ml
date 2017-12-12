open Format

open Pp
open Syntax

exception Type_error of string

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyVar (v + 1)
  in body

let tysc_of_ty u = TyScheme ([], u)

let dom = function
  | TyFun (u1, _) -> u1
  | TyDyn -> TyDyn
  | _ -> raise @@ Type_error "failed to match: dom"

let cod = function
  | TyFun (_, u2) -> u2
  | TyDyn -> TyDyn
  | _ -> raise @@ Type_error "failed to match: cod"

let rec meet u1 u2 = match u1, u2 with
  | TyBool, TyBool -> TyBool
  | TyInt, TyInt -> TyInt
  | TyVar x1, TyVar x2 when x1 = x2 -> TyVar x1
  | TyDyn, u | u, TyDyn -> u
  | TyFun (u11, u12), TyFun (u21, u22) ->
    TyFun (meet u11 u21, meet u12 u22)
  | _ -> raise @@ Type_error "failed to meet"

let type_of_binop = function
  | Plus | Minus | Mult | Div -> TyInt, TyInt, TyInt
  | Lt | Lte | Gt | Gte -> TyInt, TyInt, TyBool

let rec is_static_type = function
  | TyFun (u1, u2) -> (is_static_type u1) && (is_static_type u2)
  | TyDyn -> false
  | _ -> true

let is_static_types types = List.fold_left (&&) true @@ List.map is_static_type types

(* TODO: rename *)
let is_bvp_type = function
  | TyBool
  | TyInt
  | TyVar _ -> true
  | _ -> false

let is_tyvar = function TyVar _ -> true | _ -> false

let rec is_consistent u1 u2 = match u1, u2 with
  | TyDyn, TyDyn
  | TyBool, TyBool
  | TyInt, TyInt -> true
  | TyVar x1, TyVar x2 when x1 = x2 -> true
  | _, TyDyn
  | TyDyn, _ -> true
  | TyFun (u11, u12), TyFun (u21, u22) ->
    (is_consistent u11 u21) && (is_consistent u12 u22)
  | _ -> false

(* Substitutions for type variables *)

type substitution = tyvar * ty
type substitutions = substitution list

(* S(t) *)
let subst_type s u =
  let rec subst (x, u as s) = function
    | TyFun (u1, u2) -> TyFun (subst s u1, subst s u2)
    | TyVar x' when x = x' -> u
    | _ as u -> u
  in
  List.fold_left (fun u s0 -> subst s0 u) u s

(* S(Gamma) *)
let subst_tyenv s tyenv =
  (* TODO: Should avoid to substitute captured type variables? *)
  Environment.map (fun (TyScheme (xs, u)) -> TyScheme (xs, subst_type s u)) tyenv

module GTLC = struct
  open Pp.GTLC
  open Syntax.GTLC

  (* Constraint generation *)

  let generate_constr_cod_eq = function
    | TyVar x ->
      let x1, x2 = fresh_tyvar (), fresh_tyvar () in
      x2, Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2)))
    | TyFun (_, u2) -> u2, Constraints.empty
    | TyDyn -> TyDyn, Constraints.empty
    | _ as u ->
      raise @@ Type_error (
        asprintf "failed to generate constraints: cod(%a)" pp_ty u
      )

  let generate_constr_dom_con u1 u2 = match u1 with
    | TyVar x ->
      let x1, x2 = fresh_tyvar (), fresh_tyvar () in
      let c = Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2))) in
      Constraints.add (CConsistent (x1, u2)) c
    | TyFun (u11, _) -> Constraints.singleton @@ CConsistent (u11, u2)
    | TyDyn -> Constraints.singleton @@ CConsistent (u1, u2)
    | _ as u -> raise @@ Type_error (
        asprintf "failed to generate constraints: dom(%a)" pp_ty u
      )

  let rec generate_constr_meet u1 u2 = match u1, u2 with
    | TyBool, TyBool -> TyBool, Constraints.empty
    | TyInt, TyInt -> TyInt, Constraints.empty
    | TyDyn, u
    | u, TyDyn ->
      u, Constraints.singleton @@ CConsistent (u, TyDyn)
    | TyVar x, u
    | u, TyVar x ->
      TyVar x, Constraints.singleton @@ CConsistent (u, TyVar x)
    | TyFun (u11, u12), TyFun (u21, u22) ->
      let u1, c1 = generate_constr_meet u11 u21 in
      let u2, c2 = generate_constr_meet u12 u22 in
      TyFun (u1, u2), Constraints.union c1 c2
    | u1, u2 -> raise @@ Type_error (
        asprintf "failed to generate constraints: meet(%a, %a)"
          pp_ty u1 pp_ty u2
      )

  (* Set of type variables used for tau and let polymorphism *)

  module V = Set.Make(
    struct
      type t = tyvar
      let compare (x : tyvar) y = compare x y
    end
    )

  let rec tyvars_ty: ty -> V.t = function
    | TyVar x -> V.singleton x
    | TyFun (u1, u2) -> V.union (tyvars_ty u1) (tyvars_ty u2)
    | _ -> V.empty

  let free_tyvars_tysc: tysc -> V.t = function
    | TyScheme (xs, u) ->
      V.diff (tyvars_ty u) @@ V.of_list xs

  let free_tyvars_tyenv env =
    Environment.fold (fun _ us vars -> V.union vars (free_tyvars_tysc us)) env V.empty

  let closure_tyvars env s u =
    let free_tyvars_in_tyenv =
      List.fold_left V.union V.empty @@
      List.map (fun x -> tyvars_ty (subst_type s (TyVar x))) @@
      V.elements @@
      free_tyvars_tyenv env in
    V.diff (tyvars_ty u) free_tyvars_in_tyenv

  (* Substitutions for type variables *)

  (* S(e) *)
  let rec subst_exp s = function
    | Var (r, x, ys) -> Var (r, x, ref @@ List.map (subst_type s) !ys)
    | IConst _
    | BConst _ as e -> e
    | BinOp (r, op, e1, e2) -> BinOp (r, op, subst_exp s e1, subst_exp s e2)
    | IfExp (r, e1, e2, e3) -> IfExp (r, subst_exp s e1, subst_exp s e2, subst_exp s e3)
    | FunExp (r, x1, u1, e) -> FunExp (r, x1, subst_type s u1, subst_exp s e)
    | AppExp (r, e1, e2) -> AppExp (r, subst_exp s e1, subst_exp s e2)
    | LetExp (r, y, ys, e1, e2) ->
      (* Remove substitutions captured by let exp s *)
      let s = List.filter (fun (x, _) -> not @@ List.mem x !ys) s in
      LetExp (r, y, ys, subst_exp s e1, subst_exp s e2)

  (* [x:=t]C *)
  let subst_constraints (x: tyvar) (t: ty) (c : constr list) =
    (* [x:=t]c *)
    let subst_constraint s = function
      | CEqual (u1, u2) -> CEqual (subst_type s u1, subst_type s u2)
      | CConsistent (u1, u2) -> CConsistent (subst_type s u1, subst_type s u2)
    in
    (* TODO: OK? *)
    List.map (subst_constraint [x, t]) c

  (* Unification *)

  let rec unify = function
    | [] -> []
    | constr :: c -> begin match constr with
        | CConsistent (u1, u2) when u1 = u2 && is_bvp_type u1 ->
          unify c
        | CConsistent (TyDyn, _)
        | CConsistent (_, TyDyn) ->
          unify c
        | CConsistent (TyFun (u11, u12), TyFun (u21, u22)) ->
          unify @@ CConsistent (u11, u21) :: CConsistent (u12, u22) :: c
        | CConsistent (u, TyVar x) when not (is_tyvar u) ->
          unify @@ CConsistent (TyVar x, u) :: c
        | CConsistent (TyVar x, u) when is_bvp_type u ->
          unify @@ CEqual (TyVar x, u) :: c
        | CConsistent (TyVar x, TyFun (u1, u2)) when not @@ V.mem x (tyvars_ty (TyFun (u1, u2))) ->
          let x1, x2 = fresh_tyvar (), fresh_tyvar () in
          unify @@ CEqual (TyVar x, TyFun (x1, x2)) :: CConsistent (x1, u1) :: CConsistent (x2, u2) :: c
        | CEqual (t1, t2) when t1 = t2 && is_static_type t1 && is_bvp_type t1 ->
          unify c
        | CEqual (TyFun (t11, t12), TyFun (t21, t22)) when is_static_types [t11; t12; t21; t22] ->
          unify @@ CEqual (t11, t21) :: CEqual (t12, t22) :: c
        | CEqual (t, TyVar x) when is_static_type t && not (is_tyvar t) ->
          unify @@ CEqual (TyVar x, t) :: c
        | CEqual (TyVar x, t) when not (V.mem x (tyvars_ty t)) ->
          let s = unify @@ subst_constraints x t c in
          (x, t) :: s
        | _ ->
          raise @@ Type_error (asprintf "cannot solve a constraint: %a" pp_constr constr)
      end

  (* Type inference *)

  let constr_of_subst (s: substitutions) =
    Constraints.of_list @@ List.map (fun (x, u) -> CEqual (TyVar x, u)) s

  let rec type_of_exp env = function
    | Var (_, x, ys) ->
      begin
        try
          let TyScheme (xs, u) = Environment.find x env in
          ys := List.map (fun _ -> fresh_tyvar ()) xs;
          let s = Utils.zip xs !ys in
          (subst_type s u), []
        with Not_found ->
          raise @@ Type_error (asprintf "variable '%s' not found in the environment" x)
      end
    | IConst _ -> TyInt, []
    | BConst _ -> TyBool, []
    | BinOp (_, op, e1, e2) ->
      let ui1, ui2, ui = type_of_binop op in
      let u1, s1 = type_of_exp env e1 in
      let u2, s2 = type_of_exp env e2 in
      let c1 = constr_of_subst s1 in
      let c2 = constr_of_subst s2 in
      let c3 = Constraints.of_list [CConsistent (u1, ui1); CConsistent (u2, ui2)] in
      let c = Constraints.union c1 @@ Constraints.union c2 c3 in
      let s = unify @@ Constraints.elements c in
      (subst_type s ui), s
    | IfExp (_, e1, e2, e3) ->
      let u1, s1 = type_of_exp env e1 in
      let u2, s2 = type_of_exp env e2 in
      let u3, s3 = type_of_exp env e3 in
      let c0 = Constraints.singleton @@ CConsistent (u1, TyBool) in
      let c1 = constr_of_subst s1 in
      let c2 = constr_of_subst s2 in
      let c3 = constr_of_subst s3 in
      let u4, c4 = generate_constr_meet u2 u3 in
      let c = List.fold_left Constraints.union Constraints.empty [c0; c1; c2; c3; c4] in
      let s = unify @@ Constraints.elements c in
      (subst_type s u4), s
    | FunExp (_, x, u1, e) ->
      let u2, s2 = type_of_exp (Environment.add x (tysc_of_ty u1) env) e in
      (subst_type s2 (TyFun (u1, u2))), s2
    | AppExp (_, e1, e2) ->
      let u1, s1 = type_of_exp env e1 in
      let u2, s2 = type_of_exp env e2 in
      let c1 = constr_of_subst s1 in
      let c2 = constr_of_subst s2 in
      let u3, c3 = generate_constr_cod_eq u1 in
      let c4 = generate_constr_dom_con u1 u2 in
      let c = Constraints.union c1
        @@ Constraints.union c2
        @@ Constraints.union c3 c4 in
      let s = unify @@ Constraints.elements c in
      (subst_type s u3), s
    | LetExp (_, x, xs, e1, e2) when is_value e1 ->
      let u1, s1 = type_of_exp env e1 in
      let free_tyvars = V.elements @@ closure_tyvars env s1 u1 in
      xs := free_tyvars;
      let us1 = TyScheme (free_tyvars, u1) in
      let u2, s2 = type_of_exp (Environment.add x us1 env) e2 in
      let c = Constraints.union (constr_of_subst s1) (constr_of_subst s2) in
      let s = unify @@ Constraints.elements c in
      (subst_type s u2), s
    | LetExp (r, x, _, e1, e2) ->
      type_of_exp env @@ AppExp (r, FunExp (r, x, fresh_tyvar (), e2), e1)

  let type_of_program tyenv = function
    | Exp e ->
      let u, s = type_of_exp tyenv e in
      let tyenv = subst_tyenv s tyenv in
      (* TODO: merge subst_exp s e in type_of_exp *)
      let e = subst_exp s e in
      tyenv, Exp e, u
    | LetDecl (x, xs, e) ->
      let u, s = type_of_exp tyenv e in
      let tyenv = subst_tyenv s tyenv in
      let e = subst_exp s e in
      let free_tyvars =
        if is_value e then
          closure_tyvars tyenv s u
        else
          V.empty
      in
      xs := V.elements free_tyvars;
      let us = TyScheme (!xs, u) in
      let tyenv = Environment.add x us tyenv in
      tyenv, LetDecl (x, xs, e), u

  (* Cast insertion translation *)

  let cast f u1 u2 =
    if u1 = u2 then f  (* Omit identity cast for better performance *)
    else CC.CastExp (CC.range_of_exp f, f, u1, u2, Pos)

  let rec translate_exp env = function
    | Var (r, x, ys) -> begin
        try
          let TyScheme (xs, u) = Environment.find x env in
          let s = Utils.zip xs !ys in
          let u = subst_type s u in
          CC.Var (r, x, !ys), u
        with Not_found ->
          raise @@ Type_error "variable not found (translate_exp)"
      end
    | IConst (r, i) -> CC.IConst (r, i), TyInt
    | BConst (r, b) -> CC.BConst (r, b), TyBool
    | BinOp (r, op, e1, e2) ->
      let ui1, ui2, ui = type_of_binop op in
      let f1, u1 = translate_exp env e1 in
      let f2, u2 = translate_exp env e2 in
      CC.BinOp (r, op, cast f1 u1 ui1, cast f2 u2 ui2), ui
    | IfExp (r, e1, e2, e3) ->
      let f1, u1 = translate_exp env e1 in
      let f2, u2 = translate_exp env e2 in
      let f3, u3 = translate_exp env e3 in
      let u = meet u2 u3 in
      CC.IfExp (r, cast f1 u1 TyBool, cast f2 u2 u, cast f3 u3 u), u
    | FunExp (r, x, u1, e) ->
      let f, u2 = translate_exp (Environment.add x (tysc_of_ty u1) env) e in
      CC.FunExp (r, x, u1, f), TyFun (u1, u2)
    | AppExp (r, e1, e2) ->
      let f1, u1 = translate_exp env e1 in
      let f2, u2 = translate_exp env e2 in
      CC.AppExp (r, cast f1 u1 (TyFun (dom u1, cod u1)), cast f2 u2 (dom u1)), cod u1
    | LetExp (r, x, xs, e1, e2) when is_value e1 ->
      let f1, u1 = translate_exp env e1 in
      let us1 = TyScheme (!xs, u1) in
      let f2, u2 = translate_exp (Environment.add x us1 env) e2 in
      CC.LetExp (r, x, !xs, f1, f2), u2
    | LetExp (r, x, _, e1, e2) ->
      let _, u1 = translate_exp env e1 in
      let e = AppExp (r, FunExp (r, x, u1, e2), e1) in
      translate_exp env e

  let translate tyenv = function
    | Exp e ->
      let f, u = translate_exp tyenv e in
      CC.Exp f, u
    | LetDecl (x, xs, e) ->
      let f, u = translate_exp tyenv e in
      CC.LetDecl (x, !xs, f), u
end

module CC = struct
  open Syntax.CC

  let rec type_of_exp env = function
    | Var (_, x, ys) -> begin
        try
          let TyScheme (xs, u) = Environment.find x env in
          if List.length xs = List.length ys then
            subst_type (Utils.zip xs ys) u
          else
            raise @@ Type_error "invalid type application"
        with Not_found ->
          raise @@ Type_error "variable not found (CC.type_of_exp)"
      end
    | IConst _ -> TyInt
    | BConst _ -> TyBool
    | BinOp (_, op, f1, f2) ->
      let u1 = type_of_exp env f1 in
      let u2 = type_of_exp env f2 in
      let ui1, ui2, ui = type_of_binop op in
      if (u1, u2) = (ui1, ui2) then
        ui
      else
        raise @@ Type_error "binop"
    | IfExp (_, f1, f2, f3) ->
      let u1 = type_of_exp env f1 in
      let u2 = type_of_exp env f2 in
      let u3 = type_of_exp env f3 in
      if u1 = TyBool && u2 = u3 then
        u2
      else
        raise @@ Type_error "if"
    | FunExp (_, x, u1, f) ->
      let u2 = type_of_exp (Environment.add x (tysc_of_ty u1) env) f in
      TyFun (u1, u2)
    | AppExp (_, f1, f2) ->
      let u1 = type_of_exp env f1 in
      let u2 = type_of_exp env f2 in
      begin match u1, u2 with
        | TyFun (u11, u12), u2 when u11 = u2 ->
          u12
        | _ -> raise @@ Type_error "app"
      end
    | CastExp (_, f, u1, u2, _) ->
      let u = type_of_exp env f in
      if u = u1 then
        if is_consistent u1 u2 then
          u2
        else
          raise @@ Type_error "not consistent"
      else
        raise @@ Type_error "invalid source type"
    | LetExp (_, x, xs, f1, f2) when is_value f1 ->
      let u1 = type_of_exp env f1 in
      let us1 = TyScheme (xs, u1) in
      let u2 = type_of_exp (Environment.add x us1 env) f2 in
      u2
    | LetExp _ ->
      raise @@ Type_error "invalid translation for let expression"
    | Hole ->
      raise @@ Type_error "hole"

  let rec subst_exp s = function
    | Var (r, x, ys) -> Var (r, x, List.map (subst_type s) ys)
    | IConst _
    | BConst _ as f -> f
    | BinOp (r, op, f1, f2) -> BinOp (r, op, subst_exp s f1, subst_exp s f2)
    | IfExp (r, f1, f2, f3) -> IfExp (r, subst_exp s f1, subst_exp s f2, subst_exp s f3)
    | FunExp (r, x1, u1, f) -> FunExp (r, x1, subst_type s u1, subst_exp s f)
    | AppExp (r, f1, f2) -> AppExp (r, subst_exp s f1, subst_exp s f2)
    | CastExp (r, f, u1, u2, p) -> CastExp (r, subst_exp s f, subst_type s u1, subst_type s u2, p)
    | LetExp (r, y, ys, f1, f2) ->
      (* Remove substitutions captured by let exp s *)
      let s = List.filter (fun (x, _) -> not @@ List.mem x ys) s in
      LetExp (r, y, ys, subst_exp s f1, subst_exp s f2)
    | Hole as f -> f

  let type_of_program tyenv = function
    | Exp e -> type_of_exp tyenv e
    | LetDecl (_, _, f) -> type_of_exp tyenv f
end
