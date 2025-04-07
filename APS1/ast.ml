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
  Bool
| Int
| Void  (* pas nécessaire *)
| ASTFlech of types * typee
and types =
  ASTType of typee
| ASTTypes of typee * types

type arg = 
  Arg of string * typee
type args = 
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

type def =
  ASTConst of string * typee * expr
| ASTFun of string * typee * args * expr
| ASTFunRec of string * typee * args * expr
| ASTVar of string * typee
| ASTProc of string * args * block
| ASTProcRec of string * args * block

and stat =
  ASTEcho of expr
| ASTSet of string * expr
| ASTIf2 of expr * block * block
| ASTWhile of expr * block
| ASTCall of string * exprs
  
and cmds =
    ASTStat of stat
  | ASTDef of def * cmds
  | ASTStat2 of stat * cmds

and block =
  ASTBlock of cmds

type prog =
  ASTProg of block







	
