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
%token CONST FUN REC VAR PROC 
%token ECHO SET IF2 WHILE CALL
%token IF AND OR BOOL INT VOID

%type <Ast.expr> expr
%type <Ast.exprs> exprs
%type <Ast.cmds> cmds
%type <Ast.prog> prog
%type <Ast.block> block

%start prog

%%
prog: block            { ASTProg($1) }  
;

block: LBRA cmds RBRA  { ASTBlock($2) }
;

cmds:
  stat                          { ASTStat($1) }
| def PVIR cmds                 { ASTDef($1, $3) }
| stat PVIR cmds                { ASTStat2($1, $3) }

def:
  CONST IDENT typee expr                    { ASTConst($2, $3, $4) }
| FUN IDENT typee LBRA args RBRA expr       { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT typee LBRA args RBRA expr   { ASTFunRec($3, $4, $6, $8) }
| VAR IDENT typee                            { ASTVar($2, $3) }
| PROC IDENT LBRA args RBRA block           { ASTProc($2, $4, $6) }
| PROC REC IDENT LBRA args RBRA block       { ASTProcRec($3, $5, $7) }

typee :
  BOOL                          { Bool }
| INT                           { Int }
| VOID                          { Void }
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
| SET IDENT expr                { ASTSet($2, $3) }
| IF2 expr block block          { ASTIf2($2, $3, $4) }
| WHILE expr block              { ASTWhile($2, $3) }
| CALL IDENT exprs              { ASTCall($2, $3) }
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

