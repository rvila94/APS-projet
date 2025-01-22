(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }  
  | ';'              { PVIR }
  | ','              { VIR }
  | ':'              { DEUXP }
  | '*'              { ASTER }
  | "->"             { FLECH }
  | "CONST"          { CONST }
  | "FUN"            { FUN }  
  | "REC"            { REC }     
  | "ECHO"           { ECHO }  
  | "if"             { IF }    
  | "and"            { AND }  
  | "or"             { OR }  
  | "bool"           { BOOL }  
  | "int"            { INT }  
  | ('-')?['0'-'9']+ as lxm { NUM(int_of_string lxm) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
