%{
open Syntax
open Syntax.GTLC
open Utils.Error
%}

%token <Utils.Error.range> LPAREN RPAREN SEMI SEMISEMI COLON EQ
%token <Utils.Error.range> PLUS MINUS STAR DIV LT LTE GT GTE
%token <Utils.Error.range> LET REC IN FUN IF THEN ELSE
%token <Utils.Error.range> INT BOOL QUESTION RARROW
%token <Utils.Error.range> TRUE FALSE

%token <int Utils.Error.with_range> INTV
%token <Syntax.id Utils.Error.with_range> ID

%start toplevel
%type <Syntax.GTLC.program> toplevel

(* Ref: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)
%right SEMI
%left  EQ LT LTE GT GTE
%left  PLUS MINUS
%left  STAR DIV

%%

toplevel :
  | Expr SEMISEMI { Exp $1 }
  /* TODO: Refactor the following four rules */
  | start=LET x=ID params=LetParams EQ e=Expr SEMISEMI {
      let r = join_range start (range_of_exp e) in
      let e = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e in
      LetDecl (x.value, ref [], e)
    }
  | start=LET x=ID params=LetParams COLON u=Type EQ e=Expr SEMISEMI {
      let r = join_range start (range_of_exp e) in
      let e = AscExp (range_of_exp e, e, u) in
      let e = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e in
      LetDecl (x.value, ref [], e)
    }
  | start=LET REC x=ID params=LetRecParams EQ e=Expr SEMISEMI {
      let r = join_range start (range_of_exp e) in
      match params with
      | [] -> LetDecl (x.value, ref [], e)
      | (y, u1) :: params ->
        let e = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e in
        let u2 = List.fold_right (fun (_, u1) u -> TyFun (u1, u)) params (Typing.fresh_tyvar ()) in
        LetDecl (x.value, ref [], FixExp (r, x.value, y.value, u1, u2, e))
    }
  | start=LET REC x=ID params=LetRecParams COLON u2=Type EQ e=Expr SEMISEMI {
      let r = join_range start (range_of_exp e) in
      match params with
      | [] -> LetDecl (x.value, ref [], AscExp (r, e, u2))
      | (y, u1) :: params ->
        let e = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e in
        let u2 = List.fold_right (fun (_, u1) u -> TyFun (u1, u)) params u2 in
        LetDecl (x.value, ref [], FixExp (r, x.value, y.value, u1, u2, e))
    }

Expr :
  /* TODO: Refactor the following four rules */
  | start=LET x=ID params=LetParams EQ e1=Expr IN e2=Expr {
      let r = join_range start (range_of_exp e2) in
      let e1 = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e1 in
      LetExp (r, x.value, ref [], e1, e2)
    }
  | start=LET x=ID params=LetParams COLON u1=Type EQ e1=Expr IN e2=Expr {
      let r = join_range start (range_of_exp e2) in
      let e1 = AscExp (range_of_exp e1, e1, u1) in
      let e1 = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e1 in
      LetExp (r, x.value, ref [], e1, e2)
    }
  | start=LET REC x=ID params=LetRecParams EQ e1=Expr IN e2=Expr {
      let r = join_range start (range_of_exp e2) in
      match params with
      | [] -> LetExp (r, x.value, ref [], e1, e2)
      | (y, u1) :: params ->
        let e1 = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e1 in
        let u2 = List.fold_right (fun (_, u1) u -> TyFun (u1, u)) params (Typing.fresh_tyvar ()) in
        LetExp (r, x.value, ref [], FixExp (r, x.value, y.value, u1, u2, e1), e2)
    }
  | start=LET REC x=ID params=LetRecParams COLON u2=Type EQ e1=Expr IN e2=Expr {
      let r = join_range start (range_of_exp e2) in
      match params with
      | [] -> LetExp (r, x.value, ref [], AscExp (r, e1, u2), e2)
      | (y, u1) :: params ->
        let e1 = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e1 in
        let u2 = List.fold_right (fun (_, u1) u -> TyFun (u1, u)) params u2 in
        LetExp (r, x.value, ref [], FixExp (r, x.value, y.value, u1, u2, e1), e2)
    }
  | start=FUN params=FunParams RARROW e=Expr {
      let r = join_range start (range_of_exp e) in
      List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e
    }
  | SeqExpr { $1 }

LetRecParams :
  | params=nonempty_list(Param) { params }

LetParams :
  | params=list(Param) { params }

FunParams :
  | params=nonempty_list(Param) { params }

Param :
  | x=ID { (x, Typing.fresh_tyvar ()) }
  | LPAREN x=ID COLON u=Type RPAREN { (x, u) }

SeqExpr :
  | e1=SeqExpr SEMI e2=SeqExpr {
      let r = join_range (range_of_exp e1) (range_of_exp e2) in
      LetExp (r, "_", ref [], AscExp (range_of_exp e1, e1, TyUnit), e2)
    }
  | IfExpr { $1 }

IfExpr :
  | start=IF e1=IfExpr THEN e2=IfExpr ELSE e3=IfExpr {
      let r = join_range start (range_of_exp e3) in
      IfExp (r, e1, e2, e3)
  }
  | BinOpExpr { $1 }

BinOpExpr :
  | e1=BinOpExpr PLUS e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Plus, e1, e2)
    }
  | e1=BinOpExpr MINUS e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Minus, e1, e2)
    }
  | e1=BinOpExpr STAR e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Mult, e1, e2)
    }
  | e1=BinOpExpr DIV e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Div, e1, e2)
    }
  | e1=BinOpExpr EQ e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Eq, e1, e2)
    }
  | e1=BinOpExpr LT e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Lt, e1, e2)
    }
  | e1=BinOpExpr LTE e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Lte, e1, e2)
    }
  | e1=BinOpExpr GT e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Gt, e1, e2)
    }
  | e1=BinOpExpr GTE e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Gte, e1, e2)
    }
  | AppExpr { $1 }

AppExpr :
  | e1=AppExpr e2=SimpleExpr {
      AppExp (join_range (range_of_exp e1) (range_of_exp e2), e1, e2)
    }
  | SimpleExpr { $1 }

SimpleExpr :
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
  | LPAREN f=Expr RPAREN { f }

Type :
  | u1=AType RARROW u2=AType { TyFun (u1, u2) }
  | AType { $1 }

AType :
  | LPAREN Type RPAREN { $2 }
  | INT { TyInt }
  | BOOL { TyBool }
  | QUESTION { TyDyn }
