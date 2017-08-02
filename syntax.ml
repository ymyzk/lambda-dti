type id = string

module Environment = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
)

type op = Plus | Mult | Lt

type typaram = int
type tyvar = int
type ty =
  | TyDyn
  | TySParam of typaram
  | TyGParam of typaram
  | TyVar of tyvar
  | TyInt
  | TyBool
  | TyFun of ty * ty

let is_ground = function
  | TyGParam _ -> true
  | TyInt | TyBool -> true
  | TyFun (u1, u2) when u1 = TyDyn && u2 = TyDyn -> true
  | _ -> false

let ground_of_ty = function
  | TyGParam a -> TyGParam a
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun _ -> TyFun (TyDyn, TyDyn)
  | _ -> raise Not_found

module GTLC = struct
  type constr =
    | CEqual of ty * ty
    | CConsistent of ty * ty

  module IConstraints = Set.Make (
    struct
      type t = constr
      let compare (x : constr) y = compare x y
    end
  )

  module Constraints = struct
    include IConstraints

    let to_list c = IConstraints.fold (fun x l -> x :: l) c []
  end

  type exp =
    | Var of id
    | IConst of int
    | BConst of bool
    | BinOp of op * exp * exp
    | FunExp of id * ty * exp
    | AppExp of exp * exp

  let map_exp f_ty f_exp = function
    | Var _ as e -> e
    | IConst _ as e -> e
    | BConst _ as e -> e
    | BinOp (op, e1, e2) -> BinOp (op, f_exp e1, f_exp e2)
    | FunExp (x1, u1, e) -> FunExp (x1, f_ty u1, f_exp e)
    | AppExp (e1, e2) -> AppExp (f_exp e1, f_exp e2)
end

module CC = struct
  type exp =
    | Var of id
    | IConst of int
    | BConst of bool
    | BinOp of op * exp * exp
    | FunExp of id * ty * exp
    | AppExp of exp * exp
    | CastExp of exp * ty * ty

  let map_exp f_ty f_exp = function
    | Var _ as e -> e
    | IConst _ as e -> e
    | BConst _ as e -> e
    | BinOp (op, e1, e2) -> BinOp (op, f_exp e1, f_exp e2)
    | FunExp (x1, u1, e) -> FunExp (x1, f_ty u1, f_exp e)
    | AppExp (e1, e2) -> AppExp (f_exp e1, f_exp e2)
    | CastExp (e, u1, u2) -> CastExp (f_exp e, f_ty u1, f_ty u2)

  let rec is_value = function
    | IConst _
    | BConst _
    | FunExp _ -> true
    | CastExp (v, TyFun _, TyFun _) when is_value v -> true
    | CastExp (v, g, TyDyn) when is_value v && is_ground g -> true
    | _ -> false

  type context =
    | CTop
    | CAppL of context * exp
    | CAppR of value * context
    | CBinOpL of op * context * exp
    | CBinOpR of op * value * context
    | CCast of context * ty * ty
  and value = exp
end
