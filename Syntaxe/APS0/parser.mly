%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token PVIR DEUXP VIR ASTER FLECH
%token CONST FUN REC ECHO
%token IF AND OR BOOL INT

%type <Ast.expr> expr
%type <Ast.exprs> exprs
%type <Ast.cmds> cmds
%type <Ast.prog> prog

%start prog

%%
prog: LBRA cmds RBRA            { ASTProg($2) }
;

cmds:
  stat                          { ASTStat($1) }
| def PVIR cmds                 { ASTDef($1, $3) }
;

def:
  CONST IDENT typee expr                    { ASTConst($2, $3, $4) }
| FUN IDENT typee LBRA args RBRA expr       { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT typee LBRA args RBRA expr   { ASTFunRec($3, $4, $6, $8) }

typee :
  BOOL                          { Bool }
| INT                           { Int }
| LPAR types FLECH typee RPAR   { ASTFlech($2, $4) }

types:
  typee                         { ASTType($1) }
| typee ASTER types             { ASTTypes($1, $3) }

args:
  arg                           { ASTArg($1) }
| arg VIR args                  { ASTArgs($1, $3) }

arg:
  IDENT DEUXP typee             { Arg($1, $3) }

stat:
  ECHO expr                     { ASTEcho($2) }
;

expr:
  NUM                           { ASTNum($1) }
| IDENT                         { ASTId($1) }
| LPAR IF expr expr expr RPAR   { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR       { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR        { ASTOr($3, $4) }
| LPAR expr exprs RPAR          { ASTApp($2, $3) }
| LBRA args RBRA expr           { ASTAbs($2, $4) }
;

exprs :
  expr                          { ASTExpr($1) }
| expr exprs                    { ASTExprs($1, $2) }
;

