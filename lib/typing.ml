open Syntax

exception Type_error of string

let fresh_gparam =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyGParam (v + 1)
  in body

let fresh_sparam =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TySParam (v + 1)
  in body

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

let type_of_binop = function
  | Plus -> TyInt, TyInt, TyInt
  | Mult -> TyInt, TyInt, TyInt
  | Lt -> TyInt, TyInt, TyBool

let rec contains_static_param = function
  | TyDyn
  | TyBool
  | TyInt
  | TyVar _
  | TyGParam _ -> false
  | TySParam _ -> true
  | TyFun (u1, u2) -> (contains_static_param u1) || (contains_static_param u2)

let rec is_static_type = function
  | TyFun (u1, u2) -> (is_static_type u1) && (is_static_type u2)
  | TyDyn -> false
  | _ -> true

let is_static_types types = List.fold_left (&&) true @@ List.map is_static_type types

let is_bvp_type = function
  | TyBool
  | TyInt
  | TyVar _
  | TySParam _  (* TODO: This is OK? *)
  | TyGParam _ -> true
  | _ -> false

let is_tyvar = function TyVar _ -> true | _ -> false

let rec is_consistent u1 u2 = match u1, u2 with
  | TyDyn, TyDyn
  | TyBool, TyBool
  | TyInt, TyInt -> true
  | TySParam s1, TySParam s2 when s1 = s2 -> true
  | u, TyDyn when not (contains_static_param u) -> true
  | TyDyn, u when not (contains_static_param u) -> true
  | TyFun (u11, u12), TyFun (u21, u22) ->
    (is_consistent u11 u21) && (is_consistent u12 u22)
  | _ -> false

(* Substitutions for type variables *)

type substitution = tyvar * ty
type substitutions = substitution list

(* [x:=t]u *)

(* S(t) *)
let subst_type s u =
  let rec subst_type' x t = function
    | TyFun (u1, u2) -> TyFun (subst_type' x t u1, subst_type' x t u2)
    | TyVar x' when x = x' -> t
    | _ as u -> u
  in
  List.fold_left (fun u (x, t) -> subst_type' x t u) u s

module GTLC = struct
  open Syntax.GTLC

  (* Constraint generation *)

  let generate_constr_cod_eq = function
    | TyVar x ->
      let x1, x2 = fresh_tyvar (), fresh_tyvar () in
      x2, Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2)))
    | TyFun (_, u2) -> u2, Constraints.empty
    | TyDyn -> TyDyn, Constraints.empty
    | _ -> raise @@ Type_error "failed to generate constraints: cod()="

  let generate_constr_dom_con u1 u2 = match u1 with
    | TyVar x ->
      let x1, x2 = fresh_tyvar (), fresh_tyvar () in
      let c = Constraints.singleton @@ CEqual ((TyVar x), (TyFun (x1, x2))) in
      Constraints.add (CConsistent (x1, u2)) c
    | TyFun (u11, _) -> Constraints.singleton @@ CConsistent (u11, u2)
    | TyDyn -> Constraints.singleton @@ CConsistent (u1, u2)
    | _ -> raise @@ Type_error "failed to generate constraints: dom()~"

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

  let rec free_tyvars_exp = function
    | Var (_, _, us) ->
      List.fold_left V.union V.empty @@
        List.map tyvars_ty !us
    | BConst _
    | IConst _ -> V.empty
    | BinOp (_, _, e1, e2) ->
      V.union (free_tyvars_exp e1) (free_tyvars_exp e2)
    | FunExp (_, _, u1, e) ->
      V.union (tyvars_ty u1) @@ free_tyvars_exp e
    | AppExp (_, e1, e2) ->
      V.union (free_tyvars_exp e1) (free_tyvars_exp e2)
    | LetExp (_, _, xs, e1, e2) ->
      V.diff
        (V.union (free_tyvars_exp e1) (free_tyvars_exp e2))
        (V.of_list !xs)

  let free_tyvars_tyenv env =
    Environment.fold (fun _ us vars -> V.union vars (free_tyvars_tysc us)) env V.empty

  (* Substitutions for type variables *)

  (* S(e) *)
  let rec subst_exp s = function
    | Var (r, x, ys) -> Var (r, x, ref @@ List.map (subst_type s) !ys)
    | IConst _
    | BConst _ as e -> e
    | BinOp (r, op, e1, e2) -> BinOp (r, op, subst_exp s e1, subst_exp s e2)
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

  (* [x:=t]T *)
  let subst_tau (x: tyvar) (t: ty) (tau: ty list) =
    (* TODO: OK? *)
    List.map (subst_type [x, t]) tau

  (* Replace free type variables with fresh type parameters *)

  let generate_typaram_subst tau e =
    let free_tyvars = free_tyvars_exp e in
    let tyvars_gtp = List.fold_left V.union V.empty @@
      List.map tyvars_ty tau in
    let tyvars_stp = V.diff free_tyvars tyvars_gtp in
    List.map (fun x -> x, fresh_gparam ()) (V.elements tyvars_gtp) @
    List.map (fun x -> x, fresh_sparam ()) (V.elements tyvars_stp)

  (* Unification *)

  let rec unify tau = function
    | [] -> tau, []
    | constr :: c -> begin match constr with
        | CConsistent (u1, u2) when u1 = u2 && is_bvp_type u1 ->
          unify tau c
        | CConsistent (TyDyn, u)
        | CConsistent (u, TyDyn) ->
          unify (u :: tau) c
        | CConsistent (TyFun (u11, u12), TyFun (u21, u22)) ->
          unify tau @@ CConsistent (u11, u21) :: CConsistent (u12, u22) :: c
        | CConsistent (u, TyVar x) when not (is_tyvar u) ->
          unify tau @@ CConsistent (TyVar x, u) :: c
        | CConsistent (TyVar x, u) when is_bvp_type u ->
          unify tau @@ CEqual (TyVar x, u) :: c
        | CConsistent (TyVar x, TyFun (u1, u2)) when not @@ V.mem x (tyvars_ty (TyFun (u1, u2))) ->
          let x1, x2 = fresh_tyvar (), fresh_tyvar () in
          unify tau @@ CEqual (TyVar x, TyFun (x1, x2)) :: CConsistent (x1, u1) :: CConsistent (x2, u2) :: c
        | CEqual (t1, t2) when t1 = t2 && is_static_type t1 && is_bvp_type t1 ->
          unify tau c
        | CEqual (TyFun (t11, t12), TyFun (t21, t22)) when is_static_types [t11; t12; t21; t22] ->
          unify tau @@ CEqual (t11, t21) :: CEqual (t12, t22) :: c
        | CEqual (t, TyVar x) when is_static_type t && not (is_tyvar t) ->
          unify tau @@ CEqual (TyVar x, t) :: c
        | CEqual (TyVar x, t) when not (V.mem x (tyvars_ty t)) ->
          let tau, s = unify (subst_tau x t tau) (subst_constraints x t c) in
          tau, (x, t) :: s
        | _ ->
          raise @@ Type_error "cannot unify"
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
          (subst_type s u), [], []
        with Not_found ->
          raise @@ Type_error (Printf.sprintf "variable '%s' not found in the environment" x)
      end
    | IConst _ -> TyInt, [], []
    | BConst _ -> TyBool, [], []
    | BinOp (_, op, e1, e2) ->
      let ui1, ui2, ui = type_of_binop op in
      let u1, s1, tau1 = type_of_exp env e1 in
      let u2, s2, tau2 = type_of_exp env e2 in
      let c1 = constr_of_subst s1 in
      let c2 = constr_of_subst s2 in
      let c3 = Constraints.of_list [CConsistent (u1, ui1); CConsistent (u2, ui2)] in
      let tau = tau1 @ tau2 in
      let c = Constraints.union c1 @@ Constraints.union c2 c3 in
      let tau, s = unify tau @@ Constraints.elements c in
      (subst_type s ui), s, tau
    | FunExp (_, x, u1, e) ->
      let u2, s2, tau2 = type_of_exp (Environment.add x (tysc_of_ty u1) env) e in
      (subst_type s2 (TyFun (u1, u2))), s2, tau2
    | AppExp (_, e1, e2) ->
      let u1, s1, tau1 = type_of_exp env e1 in
      let u2, s2, tau2 = type_of_exp env e2 in
      let c1 = constr_of_subst s1 in
      let c2 = constr_of_subst s2 in
      let u3, c3 = generate_constr_cod_eq u1 in
      let c4 = generate_constr_dom_con u1 u2 in
      let tau = tau1 @ tau2 in
      let c = Constraints.union c1
        @@ Constraints.union c2
        @@ Constraints.union c3 c4 in
      let tau, s = unify tau @@ Constraints.elements c in
      (subst_type s u3), s, tau
    | LetExp (_, x, xs, e1, e2) when is_value e1 ->
      let u1, s1, tau1 = type_of_exp env e1 in (* TODO: how to handle tau for polymorphism *)
      let free_tyvars_in_tyenv =
        List.fold_left V.union V.empty @@
          List.map (fun x -> tyvars_ty (subst_type s1 (TyVar x))) @@
          V.elements @@
          free_tyvars_tyenv env in
      let free_tyvars = V.diff (tyvars_ty u1) free_tyvars_in_tyenv in
      let free_tyvars = V.elements free_tyvars in
      xs := free_tyvars;
      let us1 = TyScheme (free_tyvars, u1) in
      let u2, s2, tau2 = type_of_exp (Environment.add x us1 env) e2 in
      let tau = tau1 @ tau2 in
      let c = Constraints.union (constr_of_subst s1) (constr_of_subst s2) in
      let tau, s = unify tau @@ Constraints.elements c in
      (subst_type s u2), s, tau
    | LetExp (r, x, _, e1, e2) ->
      type_of_exp env @@ AppExp (r, FunExp (r, x, fresh_tyvar (), e2), e1)

  let type_of_program tyenv = function
    | Exp e ->
      let u, s, tau = type_of_exp tyenv e in
      let e = subst_exp s e in
      let s = generate_typaram_subst tau e in
      let e = subst_exp s e in
      let u = subst_type s u in
      Exp e, u

  (* Cast insertion translation *)

  let cast f u1 u2 =
    if u1 = u2 then f  (* Omit identity cast for better performance *)
    else CC.CastExp (CC.range_of_exp f, f, u1, u2)

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
      begin match op, u1, u2 with
        | Plus, TyInt, TyInt -> TyInt
        | Mult, TyInt, TyInt -> TyInt
        | Lt, TyInt, TyInt -> TyBool
        | _ -> raise @@ Type_error "binop"
      end
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
    | CastExp (_, f, u1, u2) ->
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
    | FunExp (r, x1, u1, f) -> FunExp (r, x1, subst_type s u1, subst_exp s f)
    | AppExp (r, f1, f2) -> AppExp (r, subst_exp s f1, subst_exp s f2)
    | CastExp (r, f, u1, u2) -> CastExp (r, subst_exp s f, subst_type s u1, subst_type s u2)
    | LetExp (r, y, ys, f1, f2) ->
      (* Remove substitutions captured by let exp s *)
      let s = List.filter (fun (x, _) -> not @@ List.mem x ys) s in
      LetExp (r, y, ys, subst_exp s f1, subst_exp s f2)
    | Hole as f -> f

  let type_of_program tyenv = function
    | Exp e -> type_of_exp tyenv e
end
