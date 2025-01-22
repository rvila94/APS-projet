type token =
  | NUM of (int)
  | IDENT of (string)
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | PVIR
  | DEUXP
  | VIR
  | ASTER
  | FLECH
  | CONST
  | FUN
  | REC
  | ECHO
  | IF
  | AND
  | OR
  | BOOL
  | INT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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

# 39 "parser.ml"
let yytransl_const = [|
  259 (* LPAR *);
  260 (* RPAR *);
  261 (* LBRA *);
  262 (* RBRA *);
  263 (* PVIR *);
  264 (* DEUXP *);
  265 (* VIR *);
  266 (* ASTER *);
  267 (* FLECH *);
  268 (* CONST *);
  269 (* FUN *);
  270 (* REC *);
  271 (* ECHO *);
  272 (* IF *);
  273 (* AND *);
  274 (* OR *);
  275 (* BOOL *);
  276 (* INT *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\003\000\003\000\006\000\006\000\006\000\007\000\007\000\
\007\000\009\000\009\000\008\000\008\000\010\000\005\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\004\000\007\000\008\000\001\000\001\000\
\005\000\001\000\003\000\001\000\003\000\003\000\002\000\001\000\
\001\000\006\000\005\000\005\000\004\000\004\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\016\000\017\000\000\000\
\000\000\015\000\001\000\000\000\000\000\007\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\021\000\
\014\000\022\000\013\000\011\000\000\000\000\000\000\000\000\000\
\019\000\020\000\009\000\005\000\000\000\018\000\006\000"

let yydgoto = "\002\000\
\043\000\044\000\008\000\004\000\009\000\010\000\035\000\032\000\
\036\000\033\000"

let yysindex = "\001\000\
\000\255\000\000\031\255\000\000\006\255\255\254\027\255\010\255\
\000\000\024\255\004\255\004\255\034\255\000\000\000\000\009\255\
\036\255\000\000\000\000\031\255\004\255\000\000\000\000\027\255\
\035\255\004\255\027\255\027\255\027\255\027\255\039\255\042\255\
\033\255\000\000\040\255\038\255\000\000\036\255\046\255\027\255\
\027\255\027\255\027\255\048\255\004\255\027\255\036\255\004\255\
\004\255\047\255\036\255\027\255\050\255\052\255\000\000\000\000\
\000\000\000\000\000\000\000\000\053\255\027\255\054\255\055\255\
\000\000\000\000\000\000\000\000\027\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\057\255\000\000\056\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\249\255\015\000\041\000\000\000\000\000\000\000\248\255\224\255\
\017\000\000\000"

let yytablesize = 67
let yytable = "\018\000\
\012\000\001\000\024\000\025\000\003\000\050\000\021\000\011\000\
\030\000\014\000\015\000\016\000\013\000\017\000\059\000\019\000\
\037\000\039\000\063\000\040\000\041\000\042\000\022\000\023\000\
\027\000\028\000\029\000\014\000\015\000\016\000\020\000\017\000\
\052\000\053\000\054\000\026\000\057\000\031\000\058\000\038\000\
\061\000\047\000\005\000\006\000\064\000\007\000\045\000\046\000\
\049\000\048\000\051\000\056\000\062\000\065\000\068\000\066\000\
\067\000\055\000\070\000\069\000\034\000\071\000\012\000\023\000\
\060\000\000\000\010\000"

let yycheck = "\007\000\
\002\001\001\000\011\000\012\000\005\001\038\000\003\001\002\001\
\016\000\001\001\002\001\003\001\014\001\005\001\047\000\006\001\
\024\000\026\000\051\000\027\000\028\000\029\000\019\001\020\001\
\016\001\017\001\018\001\001\001\002\001\003\001\007\001\005\001\
\040\000\041\000\042\000\002\001\045\000\002\001\046\000\005\001\
\049\000\009\001\012\001\013\001\052\000\015\001\008\001\006\001\
\011\001\010\001\005\001\004\001\006\001\004\001\062\000\004\001\
\004\001\043\000\004\001\006\001\020\000\069\000\006\001\004\001\
\048\000\255\255\011\001"

let yynames_const = "\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  PVIR\000\
  DEUXP\000\
  VIR\000\
  ASTER\000\
  FLECH\000\
  CONST\000\
  FUN\000\
  REC\000\
  ECHO\000\
  IF\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 32 "parser.mly"
                                ( ASTProg(_2) )
# 175 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 36 "parser.mly"
                                ( ASTStat(_1) )
# 182 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 37 "parser.mly"
                                ( ASTDef(_1, _3) )
# 190 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typee) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 41 "parser.mly"
                                            ( ASTConst(_2, _3, _4) )
# 199 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typee) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 42 "parser.mly"
                                            ( ASTFun(_2, _3, _5, _7) )
# 209 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typee) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 43 "parser.mly"
                                            ( ASTFunRec(_3, _4, _6, _8) )
# 219 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                                ( ASTBool )
# 225 "parser.ml"
               : 'typee))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                                ( ASTInt )
# 231 "parser.ml"
               : 'typee))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typee) in
    Obj.repr(
# 48 "parser.mly"
                                ( ASTFlech(_2, _4) )
# 239 "parser.ml"
               : 'typee))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typee) in
    Obj.repr(
# 51 "parser.mly"
                                ( ASTType(_1) )
# 246 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typee) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 52 "parser.mly"
                                ( ASTTypes(_1, _3) )
# 254 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 55 "parser.mly"
                                ( ASTArg(_1) )
# 261 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 56 "parser.mly"
                                ( ASTArgs(_1, _3) )
# 269 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typee) in
    Obj.repr(
# 59 "parser.mly"
                                ( Arg(_1, _3) )
# 277 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                                ( ASTEcho(_2) )
# 284 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 66 "parser.mly"
                                ( ASTNum(_1) )
# 291 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                                ( ASTId(_1) )
# 298 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                                ( ASTIf(_3, _4, _5) )
# 307 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                                ( ASTAnd(_3, _4) )
# 315 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                                ( ASTOr(_3, _4) )
# 323 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprs) in
    Obj.repr(
# 71 "parser.mly"
                                ( ASTApp(_2, _3) )
# 331 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                                ( ASTAbs(_2, _4) )
# 339 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                                ( ASTExpr(_1) )
# 346 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 77 "parser.mly"
                                ( ASTExprs(_1, _2) )
# 354 "parser.ml"
               : Ast.exprs))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
