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

  let rec generate_constr env = function
    | Var (_, x) ->
      begin
        try
          let u = Environment.find x env in
          u, Constraints.empty
        with Not_found ->
          raise @@ Type_error (Printf.sprintf "variable '%s' not found in the environment" x)
      end
    | IConst _ -> TyInt, Constraints.empty
    | BConst _ -> TyBool, Constraints.empty
    | BinOp (_, op, e1, e2) ->
      let ui1, ui2, ui = type_of_binop op in
      let u1, c1 = generate_constr env e1 in
      let u2, c2 = generate_constr env e2 in
      let c = Constraints.union c1
        @@ Constraints.union c2
        @@ Constraints.add (CConsistent (u1, ui1))
        @@ Constraints.add (CConsistent (u2, ui2))
        @@ Constraints.empty in
      ui, c
    | FunExp (_, x, u1, e) ->
      let u2, c = generate_constr (Environment.add x u1 env) e in
      TyFun (u1, u2), c
    | AppExp (_, e1, e2) ->
      let u1, c1 = generate_constr env e1 in
      let u2, c2 = generate_constr env e2 in
      let u3, c3 = generate_constr_cod_eq u1 in
      let c4 = generate_constr_dom_con u1 u2 in
      let c = Constraints.union c1
        @@ Constraints.union c2
        @@ Constraints.union c3 c4 in
      u3, c
    | LetExp (r, x, e1, e2) ->
      let e = AppExp (r, FunExp (r, x, fresh_tyvar (), e2), e1) in
      generate_constr env e

  (* Type Variables *)

  module Variables = Set.Make(
    struct
      type t = tyvar
      let compare (x : tyvar) y = compare x y
    end
    )

  let rec tyvars = function
    | TyVar x -> Variables.singleton x
    | TyFun (u1, u2) -> Variables.union (tyvars u1) (tyvars u2)
    | _ -> Variables.empty

  let rec tyvars_exp = function
    | Var _
    | BConst _
    | IConst _ -> Variables.empty
    | BinOp (_, _, e1, e2) ->
      Variables.union (tyvars_exp e1) (tyvars_exp e2)
    | FunExp (_, _, u1, e) ->
      Variables.union (tyvars u1) @@ tyvars_exp e
    | AppExp (_, e1, e2) ->
      Variables.union (tyvars_exp e1) (tyvars_exp e2)
    | LetExp (_, _, e1, e2) ->
      Variables.union (tyvars_exp e1) (tyvars_exp e2)

  (* Substitutions for type variables *)

  type substitution = tyvar * ty
  type substitutions = substitution list

  (* [x:=t]u *)
  let rec subst_type (x : tyvar) (t : ty) = function
    | TyFun (u1, u2) -> TyFun (subst_type x t u1, subst_type x t u2)
    | TyVar x' when x = x' -> t
    | _ as u -> u

  (* [x:=t]e *)
  let rec subst_exp x t e = map_exp (subst_type x t) (subst_exp x t) e

  (* S(t) *)
  let subst_type_substitutions (t : ty) (s : substitutions) =
    List.fold_left (fun u -> fun (x, t) -> subst_type x t u) t s

  (* S(e) *)
  let subst_exp_substitutions e (s : substitutions) =
    List.fold_left (fun e -> fun (x, t) -> subst_exp x t e) e s

  (* [x:=t]c *)
  let subst_constraint x t = function
    | CEqual (u1, u2) -> CEqual (subst_type x t u1, subst_type x t u2)
    | CConsistent (u1, u2) -> CConsistent (subst_type x t u1, subst_type x t u2)

  (* [x:=t]C *)
  let subst_constraints x t (c : constr list) =
    (* TODO: OK? *)
    List.map (subst_constraint x t) c

  (* [x:=t]T *)
  let subst_tau x t tau =
    (* TODO: OK? *)
    List.map (subst_type x t) tau

  (* Type Variables -> Type Parameters *)

  module TyVarMap = Map.Make(
    struct
      type t = tyvar
      let compare (x : tyvar) y = compare x y
    end
    )

  let rec subst_tyvar m = function
    | TyVar x -> TyVarMap.find x m
    | TyFun (u1, u2) -> TyFun (subst_tyvar m u1, subst_tyvar m u2)
    | _ as t -> t

  let rec subst_exp_tyvar m e = map_exp (subst_tyvar m) (subst_exp_tyvar m) e

  (* Replace type variables with type parameters *)
  let subst_tyvars tvm t =
    let f x m = if TyVarMap.mem x m then m else TyVarMap.add x (fresh_sparam ()) m in
    let tvm = Variables.fold f (tyvars t) tvm in
    tvm, subst_tyvar tvm t

  (* Replace type variables with type parameters *)
  let subst_exp_tyvars tvm e =
    let f x m = if TyVarMap.mem x m then m else TyVarMap.add x (fresh_sparam ()) m in
    let tvm = Variables.fold f (tyvars_exp e) tvm in
    tvm, subst_exp_tyvar tvm e

  (* Unification *)

  let unify constraints =
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
          | CConsistent (TyVar x, TyFun (u1, u2)) when not @@ Variables.mem x (tyvars (TyFun (u1, u2))) ->
            let x1, x2 = fresh_tyvar (), fresh_tyvar () in
            unify tau @@ CEqual (TyVar x, TyFun (x1, x2)) :: CConsistent (x1, u1) :: CConsistent (x2, u2) :: c
          | CEqual (t1, t2) when t1 = t2 && is_static_type t1 && is_bvp_type t1 ->
            unify tau c
          | CEqual (TyFun (t11, t12), TyFun (t21, t22)) when is_static_types [t11; t12; t21; t22] ->
            unify tau @@ CEqual (t11, t21) :: CEqual (t12, t22) :: c
          | CEqual (t, TyVar x) when is_static_type t && not (is_tyvar t) ->
            unify tau @@ CEqual (TyVar x, t) :: c
          | CEqual (TyVar x, t) when not (Variables.mem x (tyvars t)) ->
            let tau, s = unify (subst_tau x t tau) (subst_constraints x t c) in
            tau, (x, t) :: s
          | _ ->
            raise @@ Type_error "cannot unify"
        end
    in
    let tau, s = unify [] @@ Constraints.to_list constraints in
    let tyvars_in_tau = List.fold_left (fun vars u -> Variables.union vars (tyvars u)) Variables.empty tau in
    let tyvars_in_tau = Variables.elements tyvars_in_tau in
    s @ List.map (fun v -> (v, fresh_gparam ())) tyvars_in_tau

  (* Cast insertion translation *)

  let cast f u1 u2 =
    if u1 = u2 then f  (* Omit identity cast for better performance *)
    else CC.CastExp (CC.range_of_exp f, f, u1, u2)

  let rec translate env = function
    | Var (r, x) -> begin
        try
          let u = Environment.find x env in
          CC.Var (r, x), u
        with Not_found ->
          raise @@ Type_error "variable not found"
      end
    | IConst (r, i) -> CC.IConst (r, i), TyInt
    | BConst (r, b) -> CC.BConst (r, b), TyBool
    | BinOp (r, op, e1, e2) ->
      let ui1, ui2, ui = type_of_binop op in
      let f1, u1 = translate env e1 in
      let f2, u2 = translate env e2 in
      CC.BinOp (r, op, cast f1 u1 ui1, cast f2 u2 ui2), ui
    | FunExp (r, x, u1, e) ->
      let f, u2 = translate (Environment.add x u1 env) e in
      CC.FunExp (r, x, u1, f), TyFun (u1, u2)
    | AppExp (r, e1, e2) ->
      let f1, u1 = translate env e1 in
      let f2, u2 = translate env e2 in
      CC.AppExp (r, cast f1 u1 (TyFun (dom u1, cod u1)), cast f2 u2 (dom u1)), cod u1
    | LetExp (r, x, e1, e2) ->
      let _, u1 = translate env e1 in
      let e = AppExp (r, FunExp (r, x, u1, e2), e1) in
      translate env e
end

module CC = struct
  open Syntax.CC

  let rec type_of_exp env = function
    | Var (_, x) -> begin
        try
          Environment.find x env
        with Not_found ->
          raise @@ Type_error "variable not found"
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
      let u2 = type_of_exp (Environment.add x u1 env) f in
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
end
