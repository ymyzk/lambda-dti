%{
open Syntax
open Syntax.GTLC
open Utils.Error
%}

%token <Utils.Error.range> LPAREN RPAREN SEMISEMI COLON
%token <Utils.Error.range> PLUS STAR LT QUESTION
%token <Utils.Error.range> LET IN FUN EQ RARROW TRUE FALSE INT BOOL

%token <int Utils.Error.with_range> INTV
%token <Syntax.typaram Utils.Error.with_range> GPARAM
%token <Syntax.typaram Utils.Error.with_range> SPARAM
%token <Syntax.id Utils.Error.with_range> ID

%start toplevel
%type <Syntax.GTLC.program> toplevel

(* Ref: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)
%left  LT
%left  PLUS
%left  STAR

%%

toplevel :
  | Expr SEMISEMI { Exp $1 }
  | start=LET x=ID params=LetParams EQ e=Expr SEMISEMI {
      let r = join_range start (range_of_exp e) in
      let e = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e in
      LetDecl (x.value, ref [], e)
    }

Expr :
  | start=LET x=ID params=LetParams EQ e1=Expr IN e2=Expr {
      let r = join_range start (range_of_exp e2) in
      let e1 = List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e1 in
      LetExp (r, x.value, ref [], e1, e2)
    }
  | start=FUN params=FunParams RARROW e=Expr {
      let r = join_range start (range_of_exp e) in
      List.fold_right (fun (x, u) e -> FunExp (r, x.value, u, e)) params e
    }
  | BinOpExpr { $1 }

LetParams :
  | /* empty */ { [] }
  | params=FunParams { params }

FunParams :
  | x=ID { [x, Typing.fresh_tyvar ()] }
  | LPAREN x=ID COLON u=Type RPAREN { [x, u] }
  | x=ID rest=FunParams { (x, Typing.fresh_tyvar ()) :: rest }
  | LPAREN x=ID COLON u=Type RPAREN rest=FunParams { (x, u) :: rest }

BinOpExpr :
  | e1=BinOpExpr PLUS e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Plus, e1, e2)
    }
  | e1=BinOpExpr STAR e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Mult, e1, e2)
    }
  | e1=BinOpExpr LT e2=BinOpExpr {
      BinOp (join_range (range_of_exp e1) (range_of_exp e2), Lt, e1, e2)
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
  | x=ID { Var (x.range, x.value, ref []) }
  | LPAREN f=Expr RPAREN { f }

Type :
  | u1=AType RARROW u2=AType { TyFun (u1, u2) }
  | AType { $1 }

AType :
  | LPAREN Type RPAREN { $2 }
  | INT { TyInt }
  | BOOL { TyBool }
  | a=GPARAM { TyGParam a.value }
  | b=SPARAM { TySParam b.value }
  | QUESTION { TyDyn }
