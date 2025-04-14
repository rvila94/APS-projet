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
      Printf.printf "flech([";
      print_types ts;
      Printf.printf "],";
      print_type t1;
      Printf.printf ")";
      )
    | ASTVec(t)         -> (
      Printf.printf "vec(";
      print_type t;
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
        Printf.printf "\"%s\""  s;
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

let print_argp a =
  match a with
      Argp(s, t) ->  (
        Printf.printf "(";
        Printf.printf "\"%s\""  s;
        Printf.printf ",";
        print_type t;
        Printf.printf ")";
      )
    | ArgpVar(s, t) ->  (
        Printf.printf "var(";
        Printf.printf "\"%s\""  s;
        Printf.printf ",";
        print_type t;
        Printf.printf ")";
      )
let rec print_argsp argsp = 
  match argsp with
      ASTArgp(a)       -> print_argp a;
    | ASTArgsp(a, aas) -> (
          print_argp a;
          Printf.printf ",";
          print_argsp aas;
      )

let rec print_expr e =
  match e with
      ASTNum(n)           -> Printf.printf "num(%d)" n
    | ASTId(x)            -> Printf.printf "id(\"%s\")" x
    | ASTIf(e1, e2, e3)   -> (
      Printf.printf("if(");
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ",";
      print_expr e3;
      Printf.printf ")";
      )
    | ASTOr(e1, e2)       -> (
      Printf.printf("or(");
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf(")");
      )
    | ASTAnd(e1, e2)      -> (
      Printf.printf("and(");
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf(")");
      )
    | ASTApp(e1, es)      -> (
      Printf.printf "app(";
      print_expr e1;
      Printf.printf ",[";
      print_exprs es;
      Printf.printf "])";
      )
    | ASTAbs(args, e1)    -> (
      Printf.printf("abs([");
      print_args args;
      Printf.printf "],";
      print_expr e1;
      Printf.printf ")";
      )
    | ASTAlloc(e)         -> (
      Printf.printf("alloc(");
      print_expr e;
      Printf.printf ")";
      )
    | ASTLen(e)           -> (
      Printf.printf("len(");
      print_expr e;
      Printf.printf ")";
      )
    | ASTNth(e1, e2)      -> (
      Printf.printf("nth(");
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ")";
      )
    | ASTVset(e1, e2, e3) -> (
      Printf.printf("vset(");
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ",";
      print_expr e3;
      Printf.printf ")";
      )
      
and print_exprs es =
  match es with
      ASTExpr(e) -> print_expr e;
    | ASTExprs(e, es1) -> (
        print_expr e;
        Printf.printf ",";
        print_exprs es1;
      )

let rec print_exprp e =
  match e with
      ASTExpr(exp) -> print_expr exp
    | ASTAdr(s)    -> 
        Printf.printf("adr(");
        Printf.printf "\"%s\"" s;
        Printf.printf ")";
and print_exprsp es =
  match es with
      ASTExprp(e) -> print_exprp e;
    | ASTExprsp(e, es1) -> (
        print_exprp e;
        Printf.printf ",";
        print_exprsp es1;
      )

let rec print_stat s =
  match s with
      ASTEcho e -> (
          Printf.printf("echo(");
          print_expr(e);
          Printf.printf(")")
      )
    |  ASTSet(l, e) -> (
          Printf.printf("set(");
          print_lval l;
          Printf.printf ",";
          print_expr(e);
          Printf.printf ")";
      )
    |  ASTIf2(e, bk1, bk2) -> (
          Printf.printf("if2(");
          print_expr(e);
          Printf.printf(",");
          print_block(bk1);
          Printf.printf(",");
          print_block(bk2);
          Printf.printf ")";
      )
    |  ASTWhile(e, bk) -> (
          Printf.printf("whilee(");
          print_expr(e);
          Printf.printf(",");
          print_block(bk);
          Printf.printf ")";
      )
    |  ASTCall(s, es) -> (
          Printf.printf("call(");
          Printf.printf "\"%s\""  s;
          Printf.printf ",[";
          print_exprsp(es);
          Printf.printf "])";
      )

and print_lval l =
  match l with
    ASTLvalId(s)     ->
        Printf.printf("lvalId(");
        Printf.printf "\"%s\"" s;
        Printf.printf ")";
  | ASTLvalNth(l, e) ->
        Printf.printf("lvalNth(");
        print_lval l;
        Printf.printf ",";
        print_expr e;
        Printf.printf ")";
    

and print_def d =
  match d with 
    ASTConst(s, t, e)     -> (
        Printf.printf "const(";
        Printf.printf "\"%s\""  s;
        Printf.printf ",";
        print_type t; 
        Printf.printf ",";
        print_expr e;
        Printf.printf ")";
    )
  | ASTFun(s, t, a, e)    -> (
        Printf.printf "fun(";
        Printf.printf "\"%s\"" s;
        Printf.printf ",";
        print_type t;
        Printf.printf ",[" ;
        print_args a;
        Printf.printf "],";
        print_expr e;
        Printf.printf ")";
    )
  | ASTFunRec(s, t, a, e) -> (
        Printf.printf "funRec(";
        Printf.printf "\"%s\""  s;
        Printf.printf ",";
        print_type t;
        Printf.printf ",[";
        print_args a;
        Printf.printf "],";
        print_expr e;
        Printf.printf ")";
    )
  | ASTVar(s, t)          -> (
        Printf.printf "var(";
        Printf.printf "\"%s\""  s;
        Printf.printf ",";
        print_type t; 
        Printf.printf ")";
  )
  | ASTProc(s, a, bk)     -> (
        Printf.printf "proc(";
        Printf.printf "\"%s\"" s;
        Printf.printf ",[";
        print_argsp a;
        Printf.printf "],";
        print_block bk;
        Printf.printf ")";
    )
  | ASTProcRec(s, a, bk)  -> (
        Printf.printf "procRec(";
        Printf.printf "\"%s\"" s;
        Printf.printf ",[";
        print_argsp a;
        Printf.printf "],";
        print_block bk;
        Printf.printf ")";
    )

and print_cmds c =
  match c with
      ASTStat s           -> (
        Printf.printf "stat(";
        print_stat s;
        Printf.printf ")";
        )
    | ASTDef(d, cmds)   -> ( 
        Printf.printf "def(";
        print_def d; 
        Printf.printf "),";
        print_cmds cmds
        )
    | ASTStat2(s, cmds) -> ( 
        Printf.printf "stat(";
        print_stat s; 
        Printf.printf "),";
        print_cmds cmds
        )

and print_block bk =
  match bk with
      ASTBlock(cs) -> (
        Printf.printf "block([";
        print_cmds cs;
        Printf.printf "])";
      )

	
let print_prog p =
  match p with
      ASTProg(bk) -> (
          Printf.printf "prog(";
          print_block bk;
          Printf.printf ")"
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
      
