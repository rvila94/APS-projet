(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type typee =
  ASTBool
| ASTInt
| ASTFlech of types * typee
and types =
  ASTType of typee
| ASTTypes of typee * types

type arg = 
  Arg of string * typee
and args = 
  ASTArg of arg
| ASTArgs of arg * args

type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * exprs
  | ASTAbs of args * expr
and exprs = 
    ASTExpr of expr
  | ASTExprs of expr * exprs

type stat =
  ASTEcho of expr

type def =
    ASTConst of string * typee * expr
  | ASTFun of string * typee * args * expr
  | ASTFunRec of string * typee * args * expr

type cmds =
    ASTStat of stat
  | ASTDef of def * cmds

type prog =
  ASTProg of cmds







	
