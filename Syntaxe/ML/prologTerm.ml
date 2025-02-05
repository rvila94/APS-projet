(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

let rec print_type t =
  match t with 
      Bool           -> Printf.printf "bool"
    | Int            -> Printf.printf "int"
    | ASTFlech(ts, t1)  -> (
      Printf.printf "astflech";
      Printf.printf "([";
      print_types ts;
      Printf.printf "],";
      print_type t1;
      Printf.printf ")";
      )
and print_types ts = 
    match ts with 
        ASTType(t)        -> print_type t
      | ASTTypes(t, ts1)  -> (
          print_type t;
          Printf.printf ",";
          print_types ts1;
        )

let print_arg a =
  match a with
      Arg(s, t) ->  (
        Printf.printf "(";
        Printf.printf "%s"  s;
        Printf.printf ",";
        print_type t;
        Printf.printf ")";
      )
let rec print_args args = 
  match args with
      ASTArg(a)       -> print_arg a;
    | ASTArgs(a, aas) -> (
          print_arg a;
          Printf.printf ",";
          print_args aas;
      )

let rec print_expr e =
  match e with
      ASTNum(n)         -> Printf.printf "num(%d)" n
    | ASTId(x)          -> Printf.printf "id(%s)" x
    | ASTIf(e1, e2, e3) -> (
      Printf.printf("if(");
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ",";
      print_expr e3;
      Printf.printf ")";
      )
    | ASTOr(e1, e2)     -> (
      Printf.printf("or(");
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf(")");
      )
    | ASTAnd(e1, e2)    -> (
      Printf.printf("and(");
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf(")");
      )
    | ASTApp(e1, es)    -> (
      Printf.printf "app(\"";
      print_expr e1;
      Printf.printf "\",";
      print_exprs es;
      Printf.printf ")";
      )
    | ASTAbs(args, e1)   -> (
      Printf.printf("abs([");
      print_args args;
      Printf.printf "],";
      print_expr e1;
      Printf.printf ")";
      )
      
and print_exprs es =
  match es with
      ASTExpr(e) -> print_expr e
    | ASTExprs(e, es1) -> (
        Printf.printf "[";
        print_expr e;
        Printf.printf ",";
        print_exprs es1;
        Printf.printf "]";
      )

let print_stat s =
  match s with
      ASTEcho e -> (
          Printf.printf("echo(");
          print_expr(e);
          Printf.printf(")")
      )

let print_def d =
  match d with 
    ASTConst(s, t, e)     -> (
        Printf.printf "const(";
        Printf.printf "%s"  s;
        Printf.printf ",";
        print_type t; 
        Printf.printf ",";
        print_expr e;
        Printf.printf ")";
    )
  | ASTFun(s, t, a, e)    -> (
        Printf.printf "fun(";
        Printf.printf "%s" s;
        Printf.printf ",";
        print_type t;
        Printf.printf ",[" ;
        print_args a;
        Printf.printf "],";
        print_expr e;
        Printf.printf ")";
    )
  | ASTFunRec(s, t, a, e) -> (
        Printf.printf "funrec(";
        Printf.printf "%s"  s;
        Printf.printf ",";
        print_type t;
        Printf.printf ",[";
        print_args a;
        Printf.printf "],";
        print_expr e;
        Printf.printf ")";
    )

let rec print_cmds c =
  match c with
      ASTStat s -> (
        Printf.printf "stat(";
        print_stat s;
        Printf.printf ")";
        )
      | ASTDef(d, cmds) -> ( 
        Printf.printf "def(";
          print_def d; 
          Printf.printf "),";
          print_cmds cmds
        )
	
let print_prog p =
  match p with
      ASTProg(cs) -> (
          Printf.printf "prog([";
          print_cmds cs;
          Printf.printf "])"
      )
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      
