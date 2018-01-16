open Utils.Error

type id = string

module Environment = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
  )

type op = Plus | Minus | Mult | Div | Eq | Lt | Lte | Gt | Gte

type ty =
  | TyDyn
  | TyVar of tyvar
  | TyInt
  | TyBool
  | TyFun of ty * ty
and tyvar = ty option ref

type tysc = TyScheme of tyvar list * ty

let is_ground = function
  | TyInt | TyBool -> true
  | TyFun (u1, u2) when u1 = TyDyn && u2 = TyDyn -> true
  | _ -> false

let ground_of_ty = function
  | TyInt -> Some TyInt
  | TyBool -> Some TyBool
  | TyFun _ -> Some (TyFun (TyDyn, TyDyn))
  | _ -> None

module GTLC = struct
  type constr =
    | CEqual of ty * ty
    | CConsistent of ty * ty

  type exp =
    | Var of range * id * ty list ref
    | IConst of range * int
    | BConst of range * bool
    | BinOp of range * op * exp * exp
    | AscExp of range * exp * ty
    | IfExp of range * exp * exp * exp
    | FunExp of range * id * ty * exp
    | AppExp of range * exp * exp
    | LetExp of range * id * tyvar list ref * exp * exp

  let range_of_exp = function
    | Var (r, _, _)
    | IConst (r, _)
    | BConst (r, _)
    | AscExp (r, _, _)
    | BinOp (r, _, _, _)
    | IfExp (r, _, _, _)
    | FunExp (r, _, _, _)
    | AppExp (r, _, _)
    | LetExp (r, _, _, _, _) -> r

  (* For value restriction *)
  let is_value = function
    | IConst _
    | BConst _
    | FunExp _ -> true
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
    | BinOp of range * op * exp * exp
    | IfExp of range * exp * exp * exp
    | FunExp of range * id * ty * exp
    | AppExp of range * exp * exp
    | CastExp of range * exp * ty * ty * polarity
    | LetExp of range * id * tyvar list * exp * exp

  let range_of_exp = function
    | Var (r, _, _)
    | IConst (r, _)
    | BConst (r, _)
    | BinOp (r, _, _, _)
    | IfExp (r, _, _, _)
    | FunExp (r, _, _, _)
    | AppExp (r, _, _)
    | CastExp (r, _, _, _, _)
    | LetExp (r, _, _, _, _) -> r

  let rec is_value = function
    | IConst _
    | BConst _
    | FunExp _ -> true
    | CastExp (_, v, TyFun _, TyFun _, _) when is_value v -> true
    | CastExp (_, v, g, TyDyn, _) when is_value v && is_ground g -> true
    | _ -> false

  type program =
    | Exp of exp
    | LetDecl of id * tyvar list * exp
end
