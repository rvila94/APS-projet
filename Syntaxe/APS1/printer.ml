open Ast


(* printer de APS0, non mis a jour *)


let rec print_type t =
  match t with 
      Bool           -> Printf.printf "bool"
    | Int            -> Printf.printf "int"
    | ASTFlech(ts, t1)  -> (
      Printf.printf "( ";
      print_types ts; 
      Printf.printf " -> ";
      print_type t1;
      Printf.printf" )";
      )
and print_types ts = 
    match ts with 
        ASTType(t)        -> print_type t
      | ASTTypes(t, ts1)  -> (
          print_type t;
          Printf.printf " * ";
          print_types ts1;
        )

let print_arg a =
  match a with
      Arg(s, t) ->  (
        Printf.printf "%s"  s;
        Printf.printf " : ";
        print_type t;
      )
let rec print_args args = 
  match args with
      ASTArg(a)       -> print_arg a;
    | ASTArgs(a, aas) -> (
          print_arg a;
          Printf.printf " , ";
          print_args aas;
      )

let rec print_expr e =
  match e with
      ASTNum(n)         -> Printf.printf "%d" n
    | ASTId(x)          -> Printf.printf "%s" x
    | ASTIf(e1, e2, e3) -> (
      Printf.printf("( if ");
      print_expr e1;
      Printf.printf " ";
      print_expr e2;
      Printf.printf " ";
      print_expr e3;
      Printf.printf " )";
      )
    | ASTOr(e1, e2)     -> (
      Printf.printf("( or ");
      print_expr e1;
      Printf.printf " ";
      print_expr e2;
      Printf.printf(" )");
      )
    | ASTAnd(e1, e2)    -> (
      Printf.printf("( and ");
      print_expr e1;
      Printf.printf "  ";
      print_expr e2;
      Printf.printf(" )");
      )
    | ASTApp(e1, es)    -> (
      Printf.printf "( ";
      print_expr e1;
      Printf.printf " ";
      print_exprs es;
      Printf.printf " )";
      )
    | ASTAbs(args, e1)   -> (
      Printf.printf("[ ");
      print_args args;
      Printf.printf " ] ";
      print_expr e1;
      )
      
and print_exprs es =
  match es with
      ASTExpr(e) -> print_expr e
    | ASTExprs(e, es1) -> (
	      print_expr e;
        Printf.printf " ";
        print_exprs es1
      )

let print_stat s =
  match s with
      ASTEcho e -> (
	        Printf.printf "ECHO ";
	        print_expr e;
      )

let print_def d =
  match d with 
    ASTConst(s, t, e)     -> (
        Printf.printf "CONST ";
        Printf.printf "%s "  s;
        print_type t; 
        Printf.printf " ";
        print_expr e;
    )
  | ASTFun(s, t, a, e)    -> (
        Printf.printf "FUN ";
        Printf.printf "%s "  s;
        print_type t;
        Printf.printf " [ " ;
        print_args a;
        Printf.printf " ] ";
        print_expr e;
    )
  | ASTFunRec(s, t, a, e) -> (
        Printf.printf "FUN REC ";
        Printf.printf "%s "  s;
        print_type t;
        Printf.printf " [ ";
        print_args a;
        Printf.printf " ] ";
        print_expr e;
    )

	
let rec print_cmds cs =
  match cs with
      ASTStat(s) -> print_stat s
    | ASTDef(d, cmds) ->( 
          print_def d; 
          Printf.printf "; ";
          print_cmds cmds
      )
let print_prog p =
  match p with
      ASTProg(cs) -> (
          Printf.printf "[ ";
          print_cmds cs;
          Printf.printf " ]"
      )
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string "\n"
  with Lexer.Eof ->
    exit 0