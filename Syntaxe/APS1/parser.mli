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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
