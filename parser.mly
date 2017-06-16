%{
open Syntax
open Syntax.GTLC
%}

%token LPAREN RPAREN SEMISEMI COLON
%token PLUS STAR QUESTION
%token FUN RARROW TRUE FALSE INT BOOL
%token LT GPARAM SPARAM

%token <int> INTV
%token <Syntax.id> ID

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
  | FUN LPAREN x=ID COLON u=Type RPAREN RARROW f=Expr { FunExp (x, u, f) }
  | FUN x=ID RARROW f=Expr { FunExp (x, Typing.fresh_tyvar (), f) }
  | f1=Expr PLUS f2=Expr { BinOp (Plus, f1, f2) }
  | f1=Expr STAR f2=Expr { BinOp (Mult, f1, f2) }
  | f1=Expr LT f2=Expr { BinOp (Lt, f1, f2) }
  | AppExpr { $1 }

AppExpr :
  | f1=AppExpr f2=SimpleExpr { AppExp (f1, f2) }
  | SimpleExpr { $1 }

SimpleExpr :
  | INTV { IConst $1 }
  | TRUE { BConst true }
  | FALSE { BConst false }
  | ID { Var $1 }
  | LPAREN f=Expr RPAREN { f }

Type :
  | u1=AType RARROW u2=AType { TyFun (u1, u2) }
  | AType { $1 }

AType :
  | LPAREN Type RPAREN { $2 }
  | INT { TyInt }
  | BOOL { TyBool }
  | GPARAM a=INTV { TyGParam a }
  | SPARAM b=INTV { TySParam b }
  | QUESTION { TyDyn }
