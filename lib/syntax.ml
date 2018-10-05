open Utils.Error

type id = string

module Environment = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
  )

type binop = Plus | Minus | Mult | Div | Mod | Eq | Neq | Lt | Lte | Gt | Gte

type ty =
  | TyDyn
  | TyVar of tyvar
  | TyInt
  | TyBool
  | TyUnit
  | TyFun of ty * ty
and tyvar = ty option ref

type tysc = TyScheme of tyvar list * ty

let tysc_of_ty u = TyScheme ([], u)

let is_ground = function
  | TyInt | TyBool | TyUnit -> true
  | TyFun (u1, u2) when u1 = TyDyn && u2 = TyDyn -> true
  | _ -> false

let ground_of_ty = function
  | TyInt | TyBool | TyUnit as u -> Some u
  | TyFun _ -> Some (TyFun (TyDyn, TyDyn))
  | _ -> None

(* Set of type variables used for let polymorphism *)

module V =
  struct include Set.Make (
    struct
      type t = tyvar
      (* NOTE: We want to compare with == *)
      let compare (x : tyvar) y = compare ((Obj.magic x): int) (Obj.magic y)
  end
  )
  let big_union vars = List.fold_right union vars empty
end

let rec ftv_ty: ty -> V.t = function
  | TyVar ({ contents = None } as x) -> V.singleton x
  | TyVar ({ contents = Some u }) -> ftv_ty u
  | TyFun (u1, u2) -> V.union (ftv_ty u1) (ftv_ty u2)
  | _ -> V.empty

let ftv_tysc: tysc -> V.t = function
  | TyScheme (xs, u) -> V.diff (ftv_ty u) (V.of_list xs)

let ftv_tyenv (env: tysc Environment.t): V.t =
  Environment.fold (fun _ us vars -> V.union vars (ftv_tysc us)) env V.empty

module ITGL = struct
  type constr =
    | CEqual of ty * ty
    | CConsistent of ty * ty

  type exp =
    | Var of range * id * ty list ref
    | IConst of range * int
    | BConst of range * bool
    | UConst of range
    | BinOp of range * binop * exp * exp
    | AscExp of range * exp * ty
    | IfExp of range * exp * exp * exp
    | FunEExp of range * id * ty * exp
    | FunIExp of range * id * ty * exp
    | FixEExp of range * id * id * ty * ty * exp
    | FixIExp of range * id * id * ty * ty * exp
    | AppExp of range * exp * exp
    | LetExp of range * id * tyvar list ref * exp * exp

  let range_of_exp = function
    | Var (r, _, _)
    | IConst (r, _)
    | BConst (r, _)
    | UConst r
    | AscExp (r, _, _)
    | BinOp (r, _, _, _)
    | IfExp (r, _, _, _)
    | FunEExp (r, _, _, _)
    | FunIExp (r, _, _, _)
    | FixEExp (r, _, _, _, _, _)
    | FixIExp (r, _, _, _, _, _)
    | AppExp (r, _, _)
    | LetExp (r, _, _, _, _) -> r

  (* For value restriction *)
  let is_value = function
    | IConst _
    | BConst _
    | UConst _
    | FunEExp _
    | FunIExp _
    | FixEExp _
    | FixIExp _ -> true
    | _ -> false

  let rec ftv_exp: exp -> V.t = function
    | Var _
    | IConst _
    | BConst _
    | UConst _ -> V.empty
    | BinOp (_, _, e1, e2) -> V.union (ftv_exp e1) (ftv_exp e2)
    | AscExp (_, e, u) -> V.union (ftv_exp e) (ftv_ty u)
    | IfExp (_, e1, e2, e3) -> V.big_union @@ List.map ftv_exp [e1; e2; e3]
    | FunEExp (_, _, u, e) -> V.union (ftv_ty u) (ftv_exp e)
    | FunIExp (_, _, _, e) -> ftv_exp e
    | FixEExp (_, _, _, u1, _, e) -> V.union (ftv_ty u1) (ftv_exp e)
    | FixIExp (_, _, _, _, _, e) -> ftv_exp e
    | AppExp (_, e1, e2) -> V.union (ftv_exp e1) (ftv_exp e2)
    | LetExp (_, _, _, e1, e2) -> V.union (ftv_exp e1) (ftv_exp e2)

  type program =
    | Exp of exp
    | LetDecl of id * tyvar list ref * exp
end

module CC = struct
  type polarity = Pos | Neg

  let neg = function Pos -> Neg | Neg -> Pos

  type exp =
    | Var of range * id * ty list
    | IConst of range * int
    | BConst of range * bool
    | UConst of range
    | BinOp of range * binop * exp * exp
    | IfExp of range * exp * exp * exp
    | FunExp of range * id * ty * exp
    | FixExp of range * id * id * ty * ty * exp
    | AppExp of range * exp * exp
    | CastExp of range * exp * ty * ty * polarity
    | LetExp of range * id * tyvar list * exp * exp

  let range_of_exp = function
    | Var (r, _, _)
    | IConst (r, _)
    | BConst (r, _)
    | UConst r
    | BinOp (r, _, _, _)
    | IfExp (r, _, _, _)
    | FunExp (r, _, _, _)
    | FixExp (r, _, _, _, _, _)
    | AppExp (r, _, _)
    | CastExp (r, _, _, _, _)
    | LetExp (r, _, _, _, _) -> r

  let rec is_value = function
    | IConst _
    | BConst _
    | UConst _
    | FunExp _
    | FixExp _-> true
    | CastExp (_, v, TyFun _, TyFun _, _) when is_value v -> true
    | CastExp (_, v, g, TyDyn, _) when is_value v && is_ground g -> true
    | _ -> false

  let rec ftv_exp: exp -> V.t = function
    | Var (_, _, us) -> List.fold_right V.union (List.map ftv_ty us) V.empty
    | IConst _
    | BConst _
    | UConst _ -> V.empty
    | BinOp (_, _, f1, f2) -> V.union (ftv_exp f1) (ftv_exp f2)
    | IfExp (_, f1, f2, f3) -> List.fold_right V.union (List.map ftv_exp [f1; f2; f3]) V.empty
    | FunExp (_, _, u, e) -> V.union (ftv_ty u) (ftv_exp e)
    | FixExp (_, _, _, u1, _, f) -> V.union (ftv_ty u1) (ftv_exp f)
    | AppExp (_, f1, f2) -> V.union (ftv_exp f1) (ftv_exp f2)
    | CastExp (_, f, u1, u2, _) ->
        V.union (ftv_exp f) @@ V.union (ftv_ty u1) (ftv_ty u2)
    | LetExp (_, _, xs, f1, f2) ->
        V.union (V.diff (ftv_exp f1) (V.of_list xs)) (ftv_exp f2)

  type program =
    | Exp of exp
    | LetDecl of id * tyvar list * exp

  type tag = I | B | U | Ar

  let tag_to_ty = function
    | I -> TyInt
    | B -> TyBool
    | U -> TyUnit
    | Ar -> TyFun (TyDyn, TyDyn)

  type value =
    | IntV of int
    | BoolV of bool
    | UnitV
    | FunV of ((tyvar list * ty list) -> value -> value)
    | Tagged of tag * value
end
