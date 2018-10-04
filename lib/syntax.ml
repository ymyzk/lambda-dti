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
