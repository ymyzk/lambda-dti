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
%type <Syntax.GTLC.exp> toplevel

(* Ref: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)
%left  LT
%left  PLUS
%left  STAR

%%

toplevel :
  | Expr SEMISEMI { $1 }

Expr :
  | start=LET x=ID EQ e1=Expr IN e2=Expr {
      LetExp (join_range start (range_of_exp e2), x.value, e1, e2)
    }
  | start=FUN LPAREN x=ID COLON u=Type RPAREN RARROW e=Expr {
      FunExp (join_range start (range_of_exp e), x.value, u, e)
    }
  | start=FUN x=ID RARROW e=Expr {
      FunExp (join_range start (range_of_exp e), x.value, Typing.fresh_tyvar (), e)
    }
  | BinOpExpr { $1 }

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
  | x=ID { Var (x.range, x.value) }
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
