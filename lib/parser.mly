%{
open Syntax
open Syntax.ITGL
open Utils.Error

let tyvenv = ref Environment.empty

let param_to_fun r (x, u) e = match u with
| None -> FunIExp (r, x.value, Typing.fresh_tyvar (), e)
| Some u -> FunEExp (r, x.value, u, e)

let param_to_fun_ty r (x, u1) (e, u) = match u1 with
| None ->
    let u1 = Typing.fresh_tyvar () in
    FunIExp (r, x.value, u1, e), TyFun (u1, u)
| Some u1 ->
    FunEExp (r, x.value, u1, e), TyFun (u1, u)
%}

%token <Utils.Error.range> LPAREN RPAREN SEMI SEMISEMI COLON EQ QUOTE
%token <Utils.Error.range> PLUS MINUS STAR DIV MOD LT LTE GT GTE NEQ LAND LOR
%token <Utils.Error.range> LET REC IN FUN IF THEN ELSE
%token <Utils.Error.range> INT BOOL UNIT QUESTION RARROW
%token <Utils.Error.range> TRUE FALSE

%token <int Utils.Error.with_range> INTV
%token <Syntax.id Utils.Error.with_range> ID

%start toplevel
%type <Syntax.ITGL.program> toplevel

(* Ref: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)
%right SEMI
%right prec_if
%right LOR
%right LAND
%left  EQ NEQ LT LTE GT GTE
%left  PLUS MINUS
%left  STAR DIV MOD

%%

toplevel :
  | p=Program {
      tyvenv := Environment.empty;
      p
    }

Program :
  | Expr SEMISEMI { Exp $1 }
  | start=LET x=ID params=list(Param) u=Let_type_annot EQ e=Expr SEMISEMI {
      let r = join_range start (range_of_exp e) in
      let e = match u with None -> e | Some u -> AscExp (range_of_exp e, e, u) in
      let e = List.fold_right (param_to_fun r) params e in
      LetDecl (x.value, e)
    }
  | start=LET REC x=ID params=list(Param) u2=Let_rec_type_annot EQ e=Expr SEMISEMI {
      let r = join_range start (range_of_exp e) in
      match params with
      | [] -> LetDecl (x.value, AscExp (r, e, u2))
      | (y, None) :: params ->
        let u1 = Typing.fresh_tyvar () in
        let e, u2 = List.fold_right (param_to_fun_ty r) params (e, u2) in
        LetDecl (x.value, FixIExp (r, x.value, y.value, u1, u2, e))
      | (y, Some u1) :: params ->
        let e, u2 = List.fold_right (param_to_fun_ty r) params (e, u2) in
        LetDecl (x.value, FixEExp (r, x.value, y.value, u1, u2, e))
    }

Expr :
  | start=LET x=ID params=list(Param) u1=Let_type_annot EQ e1=Expr IN e2=Expr {
      let r = join_range start (range_of_exp e2) in
      let e1 = match u1 with None -> e1 | Some u1 -> AscExp (range_of_exp e1, e1, u1) in
      let e1 = List.fold_right (param_to_fun r) params e1 in
      LetExp (r, x.value, e1, e2)
    }
  | start=LET REC x=ID params=list(Param) u2=Let_rec_type_annot EQ e1=Expr IN e2=Expr {
      let r = join_range start (range_of_exp e2) in
      match params with
      | [] -> LetExp (r, x.value, AscExp (r, e1, u2), e2)
      | (y, None) :: params ->
        let u1 = Typing.fresh_tyvar () in
        let e1, u2 = List.fold_right (param_to_fun_ty r) params (e1, u2) in
        LetExp (r, x.value, FixIExp (r, x.value, y.value, u1, u2, e1), e2)
      | (y, Some u1) :: params ->
        let e1, u2 = List.fold_right (param_to_fun_ty r) params (e1, u2) in
        LetExp (r, x.value, FixEExp (r, x.value, y.value, u1, u2, e1), e2)
    }
  | start=FUN params=nonempty_list(Param) RARROW e=Expr {
      let r = join_range start (range_of_exp e) in
      List.fold_right (param_to_fun r) params e
    }
  | Seq_expr { $1 }

Param :
  | x=ID { (x, None) }
  | LPAREN x=ID COLON u=Type RPAREN { (x, Some u) }

%inline Let_type_annot :
  | /* empty */ { None }
  | COLON u=Type { Some u }

%inline Let_rec_type_annot :
  | /* empty */ { Typing.fresh_tyvar () }
  | COLON u2=Type { u2 }

Seq_expr :
  | e1=Seq_expr SEMI e2=Seq_expr {
      let r = join_range (range_of_exp e1) (range_of_exp e2) in
      LetExp (r, "_", AscExp (range_of_exp e1, e1, TyUnit), e2)
    }
  | start=IF e1=Seq_expr THEN e2=Seq_expr ELSE e3=Seq_expr %prec prec_if {
      let r = join_range start (range_of_exp e3) in
      IfExp (r, e1, e2, e3)
  }
  | e1=Seq_expr LOR e2=Seq_expr {
      let r = join_range (range_of_exp e1) (range_of_exp e2) in
      let t, f = BConst (r, true), BConst (r, false) in
      IfExp (r, e1, t, IfExp (r, e2, t, f))
    }
  | e1=Seq_expr LAND e2=Seq_expr {
      let r = join_range (range_of_exp e1) (range_of_exp e2) in
      let t, f = BConst (r, true), BConst (r, false) in
      IfExp (r, e1, IfExp (r, e2, t, f), f)
    }
  | e1=Seq_expr op=Op e2=Seq_expr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), op, e1, e2)
    }
  | Unary_expr { $1 }

%inline Op :
  | PLUS { Plus }
  | MINUS { Minus }
  | STAR { Mult }
  | DIV { Div }
  | MOD { Mod }
  | EQ { Eq }
  | NEQ { Neq }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }

Unary_expr :
  | PLUS e=Unary_expr { e }
  | start_r=MINUS e=Unary_expr {
      let r = join_range start_r (range_of_exp e) in
      let zero = IConst (dummy_range, 0) in
      BinOp (r, Minus, zero, e)
    }
  | App_expr { $1 }

App_expr :
  | e1=App_expr e2=Simple_expr {
      AppExp (join_range (range_of_exp e1) (range_of_exp e2), e1, e2)
    }
  | Simple_expr { $1 }

Simple_expr :
  | i=INTV { IConst (i.range, i.value) }
  | r=TRUE { BConst (r, true) }
  | r=FALSE { BConst (r, false) }
  | start=LPAREN last=RPAREN {
      UConst (join_range start last)
    }
  | x=ID { Var (x.range, x.value, ref []) }
  | start=LPAREN e=Expr COLON u=Type last=RPAREN {
      AscExp (join_range start last, e, u)
    }
  | LPAREN e=Expr RPAREN { e }

Type :
  | u1=Simple_type RARROW u2=Type { TyFun (u1, u2) }
  | Simple_type { $1 }

Simple_type :
  | INT { TyInt }
  | BOOL { TyBool }
  | UNIT { TyUnit }
  | QUESTION { TyDyn }
  | QUOTE x=ID {
      try
        Environment.find x.value !tyvenv
      with Not_found ->
        let u = Typing.fresh_tyvar () in
        tyvenv := Environment.add x.value u !tyvenv;
        u
    }
  | LPAREN u=Type RPAREN { u }
