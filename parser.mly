%{
open Syntax
open Syntax.GTLC
open Utils.Error
%}

%token <Utils.Error.range> LPAREN RPAREN SEMISEMI COLON
%token <Utils.Error.range> PLUS STAR LT QUESTION
%token <Utils.Error.range> FUN RARROW TRUE FALSE INT BOOL

%token <int Utils.Error.with_range> INTV
%token <Syntax.typaram Utils.Error.with_range> GPARAM
%token <Syntax.typaram Utils.Error.with_range> SPARAM
%token <Syntax.id Utils.Error.with_range> ID

%start toplevel
%type <Syntax.GTLC.exp> toplevel

(* Ref: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)
%right RARROW
%left  LT
%left  PLUS
%left  STAR

%%

toplevel :
  | Expr SEMISEMI { $1 }

Expr :
  | FUN LPAREN x=ID COLON u=Type RPAREN RARROW f=Expr { FunExp (x.value, u, f) }
  | FUN x=ID RARROW f=Expr { FunExp (x.value, Typing.fresh_tyvar (), f) }
  | f1=Expr PLUS f2=Expr { BinOp (Plus, f1, f2) }
  | f1=Expr STAR f2=Expr { BinOp (Mult, f1, f2) }
  | f1=Expr LT f2=Expr { BinOp (Lt, f1, f2) }
  | AppExpr { $1 }

AppExpr :
  | f1=AppExpr f2=SimpleExpr { AppExp (f1, f2) }
  | SimpleExpr { $1 }

SimpleExpr :
  | i=INTV { IConst i.value }
  | TRUE { BConst true }
  | FALSE { BConst false }
  | x=ID { Var x.value }
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
