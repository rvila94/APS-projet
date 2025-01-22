open Ast
  
let rec print_expr e =
  match e with
      ASTNum(n)         -> Printf.printf("%d" n)
    | ASTId(x)          -> Printf.printf("%s" x)
    | ASTIf(e1, e2, e3) -> Printf.printf("TODO (if)")
    | ASTOr(e1, e2)     -> Printf.printf("TODO (or)")
    | ASTAnd(e1, e2)    -> Printf.printf("TODO (and)")
    | ASTApp(e1, es)     -> (
          Printf.printf"app(";
          print_expr e1;
          Printf.printf",(";
          print_exprs es;
          Printf.printf")"
    )
    | ASTAbs(args, e1)   -> Printf.printf("TODO (abstraction fonctionelle)")
      
and print_exprs es =
  match es with
      ASTExpr(e) -> print_expr e
    | ASTExprs(e, es1) -> (
	      print_expr e;
        print_char ',';
        print_exprs es
    )

let print_stat s =
  match s with
      ASTEcho e -> (
	        Printf.printf("ECHO");
	        print_expr(e);
    )
	
let rec print_cmds cs =
  match cs with
      ASTStat(s) -> print_stat s
    | ASTDef(d, cmds) ->( 
          print_def d; 
          print_cmds cmds
    )
let print_prog p =
  Printf.printf("[");
  print_cmds p;
  Printf.printf("]")
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