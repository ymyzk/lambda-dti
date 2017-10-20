open Utils.Error

type id = string

module Environment = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
  )

type op = Plus | Minus | Mult | Div | Lt | Lte | Gt | Gte

type tyvar = int
type ty =
  | TyDyn
  | TyVar of tyvar
  | TyInt
  | TyBool
  | TyFun of ty * ty

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

  module Constraints = Set.Make (
    struct
      type t = constr
      let compare (x : constr) y = compare x y
    end
    )

  type exp =
    | Var of range * id * ty list ref
    | IConst of range * int
    | BConst of range * bool
    | BinOp of range * op * exp * exp
    | IfExp of range * exp * exp * exp
    | FunExp of range * id * ty * exp
    | AppExp of range * exp * exp
    | LetExp of range * id * tyvar list ref * exp * exp

  let map_exp f_ty f_exp = function
    | Var (r, x, ys) -> Var (r, x, ref @@ List.map f_ty !ys)
    | IConst _ as e -> e
    | BConst _ as e -> e
    | BinOp (r, op, e1, e2) -> BinOp (r, op, f_exp e1, f_exp e2)
    | IfExp (r, e1, e2, e3) -> IfExp (r, f_exp e1, f_exp e2, f_exp e3)
    | FunExp (r, x1, u1, e) -> FunExp (r, x1, f_ty u1, f_exp e)
    | AppExp (r, e1, e2) -> AppExp (r, f_exp e1, f_exp e2)
    | LetExp (r, x, xs, e1, e2) -> LetExp (r, x, xs, f_exp e1, f_exp e2)

  let range_of_exp = function
    | Var (r, _, _)
    | IConst (r, _)
    | BConst (r, _)
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
    | Hole  (* Only used during evaluation *)

  let map_exp f_ty f_exp = function
    | Var _
    | IConst _
    | BConst _ as f -> f
    | BinOp (r, op, f1, f2) -> BinOp (r, op, f_exp f1, f_exp f2)
    | IfExp (r, f1, f2, f3) -> IfExp (r, f_exp f1, f_exp f2, f_exp f3)
    | FunExp (r, x1, u1, f) -> FunExp (r, x1, f_ty u1, f_exp f)
    | AppExp (r, f1, f2) -> AppExp (r, f_exp f1, f_exp f2)
    | CastExp (r, f, u1, u2, p) -> CastExp (r, f_exp f, f_ty u1, f_ty u2, p)
    | LetExp (r, x, xs, f1, f2) -> LetExp (r, x, xs, f_exp f1, f_exp f2)
    | Hole as f -> f

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
    | Hole -> raise Not_found

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

  type context =
    | CTop
    | CAppL of range * context * exp
    | CAppR of range * value * context
    | CBinOpL of range * op * context * exp
    | CBinOpR of range * op * value * context
    | CIf of range * context * exp * exp
    | CCast of range * context * ty * ty * polarity
  and value = exp
end
