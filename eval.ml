open Format
open Syntax
open Syntax.CC

exception Blame
exception Reduce

(* Gradual Type Parameters Substitution *)

(* [a:->u] *)
type substitution = typaram * ty
(* if S = [a1:->a2], [a2:->u1], then S(a1)=u1 *)
type substitutions = substitution list

(* s(u) *)
let subst_gtp_in_type s u =
  let rec subst_gtp (a, u as s) = function
    | TyFun (u1, u2) -> TyFun (subst_gtp s u1, subst_gtp s u2)
    | TyGParam a' when a = a' -> u
    | _ as u -> u
  in
  List.fold_left (fun u -> fun s0 -> subst_gtp s0 u) u s

(* s(e) *)
let rec subst_gtp_in_exp s e =
  map_exp (fun u -> subst_gtp_in_type s u) (subst_gtp_in_exp s) e

(* s(c) *)
let rec subst_gtp_in_context s = function
  | CTop -> CTop
  | CAppL (c, e) -> CAppL (subst_gtp_in_context s c, subst_gtp_in_exp s e)
  | CAppR (e, c) -> CAppR (subst_gtp_in_exp s e, subst_gtp_in_context s c)
  | CBinOpL (op, c, e) -> CBinOpL (op, subst_gtp_in_context s c, subst_gtp_in_exp s e)
  | CBinOpR (op, e, c) -> CBinOpR (op, subst_gtp_in_exp s e, subst_gtp_in_context s c)
  | CCast (c, u1, u2) -> CCast (subst_gtp_in_context s c, subst_gtp_in_type s u1, subst_gtp_in_type s u2)

(* Reduction *)

(* e[x:=v] *)
let rec subst_var x v e =
  assert (is_value v);
  let subst = subst_var x v in
  match e with
  | Var y -> if x = y then v else e
  | IConst _
  | BConst _ -> e
  | BinOp (op, e1, e2) -> BinOp (op, subst e1, subst e2)
  | FunExp (y, u, e') ->
      if x = y then e else FunExp (y, u, subst e')
  | AppExp (e1, e2) -> AppExp (subst e1, subst e2)
  | CastExp (e1, u1, u2) -> CastExp (subst e1, u1, u2)

let reduce = function
  (* R_Beta *)
  | AppExp (FunExp (x, _, e), v) when is_value v ->
      subst_var x v e, None
  (* R_Op *)
  | BinOp (Plus, IConst i1, IConst i2) ->
      IConst (i1 + i2), None
  | BinOp (Mult, IConst i1, IConst i2) ->
      IConst (i1 * i2), None
  | BinOp (Lt, IConst i1, IConst i2) ->
      BConst (i1 < i2), None
  (* R_IdBase *)
  | CastExp (v, TyBool, TyBool) when is_value v -> v, None
  | CastExp (v, TyInt, TyInt) when is_value v -> v, None
  (* R_IdStar *)
  | CastExp (v, TyDyn, TyDyn) when is_value v -> v, None
  (* R_AppCast *)
  | AppExp (CastExp (v1, TyFun (u11, u12), TyFun (u21, u22)), v2) when is_value v1 && is_value v2 ->
      CastExp (AppExp (v1, CastExp (v2, u21, u11)), u12, u22), None
  | CastExp (CastExp (v, u1, u2), u2', u3) when is_value v && u2 = u2' ->
      begin match (u1, u2, u3) with
      (* R_Succeed *)
      | g1, TyDyn, g2 when is_ground g1 && is_ground g2 && g1 = g2 -> v, None
      (* R_Instantiate4 *)
      | TyBool, TyDyn, TyGParam a -> v, Some (a, TyBool)
      | TyInt, TyDyn, TyGParam a -> v, Some (a, TyInt)
      (* R_Instantiate5 *)
      | TyFun (TyDyn, TyDyn), TyDyn, TyGParam a ->
          let a1, a2 = Typing.fresh_gparam (), Typing.fresh_gparam () in
          CastExp (v, TyFun (TyDyn, TyDyn), TyFun (a1, a2)), Some (a, TyFun (a1, a2))
      (* R_Fail *)
      | g1, TyDyn, g2 when is_ground g1 && is_ground g2 && g1 <> g2 -> raise Blame
      | _ -> raise Reduce
      end
  (* R_Ground *)
  | CastExp (v, u, TyDyn) when u <> TyDyn && u <> ground_of_ty u ->
      let g = ground_of_ty u in
      CastExp (CastExp(v, u, g), g, TyDyn), None
  (* R_Expand *)
  | CastExp (v, TyDyn, u) when u <> TyDyn && u <> ground_of_ty u ->
      let g = ground_of_ty u in
      CastExp (CastExp(v, TyDyn, g), g, u), None
  | _ -> raise Reduce

(* Evaluation *)

let rec fill_context = function
  | CTop, e -> e
  | CAppL (c, e2), e1 -> fill_context (c, AppExp (e1, e2))
  | CAppR (e1, c), e2 -> fill_context (c, AppExp (e1, e2))
  | CBinOpL (op, c, e2), e1 -> fill_context (c, BinOp (op, e1, e2))
  | CBinOpR (op, e1, c), e2 -> fill_context (c, BinOp (op, e1, e2))
  | CCast (c, u1, u2), e -> fill_context (c, CastExp (e, u1, u2))

exception Error of context * exp
exception Value

let hole = IConst (-12345)
let rec exp_with exp_in_hole ppf =
  let exp = exp_with exp_in_hole in
  function
    | IConst _ as c when c = hole ->
        fprintf ppf "[ %a ]"
          (exp_with hole)
          exp_in_hole
    | Var x -> pp_print_string ppf x
    | BConst b -> pp_print_bool ppf b
    | IConst i -> pp_print_int ppf i
    | BinOp (op, f1, f2) ->
        fprintf ppf "(%a %a %a)"
          exp f1
          Pp.pp_binop op
          exp f2
    | FunExp (x1, u1, f) ->
        fprintf ppf "fun (%s: %a) -> %a"
          x1
          Pp.pp_ty u1
          exp f
    | AppExp (f1, f2) ->
        fprintf ppf "((%a) (%a))"
          exp f1
          exp f2
    | CastExp (f, u1, u2) ->
        fprintf ppf "(%a: %a => %a)"
          exp f
          Pp.pp_ty u1
          Pp.pp_ty u2

let pp_context_exp ppf (c, e) =
  exp_with e ppf @@ fill_context (c, hole)

let rec decompose_down (c, e as ce) =
  (* fprintf std_formatter "decompose_down %d <-- %a\n" x pp_context_exp ce; *)
  let ce =
    match e with
  | Var _ -> raise @@ Error (c, e)
  | IConst _
  | BConst _ -> raise Value
  | BinOp (op, e1, e2) -> begin
      try decompose_down (CBinOpL (op, c, e2), e1)
      with Value ->
        try decompose_down (CBinOpR (op, e1, c), e2)
        with Value -> ce
    end
  | FunExp _ -> raise Value
  | AppExp (e1, e2) -> begin
      try decompose_down (CAppL (c, e2), e1)
      with Value ->
        try decompose_down (CAppR (e1, c), e2)
        with Value -> ce
    end
  | CastExp (v, g, TyDyn) when is_value v && is_ground g -> raise Value
  | CastExp (v, TyFun _, TyFun _) when is_value v -> raise Value
  | CastExp (e1, u1, u2) ->
      try decompose_down (CCast (c, u1, u2), e1)
      with Value -> ce
  in
  (* fprintf std_formatter "decompose_down %d --> %a\n" x pp_context_exp ce; *)
  ce

let rec decompose_up (c, v) =
  (* fprintf std_formatter "decompose_up <-- %a\n" pp_context_exp (c, v); *)
  if is_value v then
    match c with
    | CTop -> raise Not_found
    | CAppL (c', e) -> begin
        try decompose_down (CAppR (v, c'), e)
        with Value -> c', AppExp (v, e)
      end
    | CAppR (v', c') -> c', AppExp (v', v)
    | CBinOpL (op, c', e) -> begin
        try decompose_down (CBinOpR (op, v, c'), e)
        with Value -> c', BinOp (op, v, e)
      end
    | CBinOpR (op, v', c') -> c', BinOp (op, v', v)
    | CCast (c', u1, u2) ->
        decompose_up (c', CastExp (v, u1, u2))
  else
    c, v

let decompose ce =
  (* fprintf std_formatter "decompose <-- %a\n" pp_context_exp ce; *)
  let result =
    try decompose_down ce
    with Value -> decompose_up ce
  in
  (* fprintf std_formatter "decompose --> %a\n" pp_context_exp result; *)
  result

let reduce_in (c, e) =
  (* fprintf std_formatter "reduce <-- %a\n" Pp.CC.pp_exp e; *)
  let e, s = reduce e in
  (* fprintf std_formatter "reduce --> %a\n" Pp.CC.pp_exp e; *)
  c, e, s

let eval_step (ce: context * exp): (context * exp) * substitution option =
  let c, e, s = reduce_in @@ decompose ce in
  match s with
  | Some s -> (subst_gtp_in_context [s] c, subst_gtp_in_exp [s] e), Some s
  | None -> (c, e), None

let rec eval_all (ce: context * exp) (ss: substitutions): (context * exp) * substitutions =
  try
    let ce, s = eval_step ce in
    let ss = begin
      match s with
      | None -> ss
      | Some s -> s :: ss
    end in
    eval_all ce ss
  with Not_found -> ce, ss

let eval (e: exp): exp * substitutions =
  let e, s = eval_all (CTop, e) [] in
  let e = fill_context e in
  e, List.rev s
