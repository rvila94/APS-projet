open Ast

type valeur =
    InZ  of int
  | InF  of expr * string list * environnement
  | InFR of expr * string list * string * environnement
  | InPrim of (valeur list -> valeur)

and environnement = (string * valeur) list

let initial_env =
  let not = InPrim (function
      [InZ(0)] -> InZ(1)
    | [InZ(1)] -> InZ(0)
    | _        -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
  ) in
  let eq  = InPrim (function
      [InZ n1; InZ n2] -> InZ (if n1 = n2 then 1 else 0)
    | _ -> failwith "erreur: mauvais types, doivent etre des entiers"
  ) in
  let lt  = InPrim (function
      [InZ n1; InZ n2] -> InZ (if n1 < n2 then 1 else 0)
    | _ -> failwith "erreur: mauvais types, doivent etre des entiers"
  ) in
  let add = InPrim (function
      [InZ n1; InZ n2] -> InZ (n1 + n2)
    | _ -> failwith "erreur: mauvais types, doivent etre des entiers"
  ) in
  let prim_sub = InPrim (function
      [InZ n1; InZ n2] -> InZ (n1 - n2)
    | _ -> failwith "erreur: mauvais types, doivent etre des entiers"
  ) in
  let prim_mul = InPrim (function
    | [InZ n1; InZ n2] -> InZ (n1 * n2)
    | _ -> failwith "erreur: mauvais types, doivent etre des entiers"
  ) in
  let prim_div = InPrim (function
      [InZ n1; InZ n2] -> InZ (n1 / n2)
    | _ -> failwith "erreur: mauvais types, doivent etre des entiers"
  ) in

  [("not", not);
  ("eq", eq);
  ("lt", lt);
  ("add", add);
  ("sub", sub);
  ("mul", mul);
  ("div", div);]

  
let rec check_env id env =
  match env with
      []                -> failwith "erreur: pas trouvé "^id^" dans l'environnement global"
    | (ident, value)::s -> if (String.equal id ident) then value else check_env id s

let rec add_env id val0 env =
  match env with
      []                ->  [(id, val0)]
    | (ident, value)::s ->  if (String.equal id ident) then (ident,val0)::s
                            else (ident,value)::(add_env id val0 s)



let rec eval_expr e env = 
  match e with 
      ASTNum(n)           -> InZ(n)
    | ASTId(s)            -> check_env s env
    | ASTIf(cond, e1,e2)  ->
                     match (eval_expr cond env) with
                        InZ(1) -> e1
                      | InZ(0) -> e2
                      | _      -> failwith "erreur: la condition doit etre un bool ( 0 ou 1 )"

    | ASTAnd(e1, e2)      -> 
                    match (eval_expr e1 env) with
                        InZ(0) -> InZ(0)
                      | InZ(1) -> 
                              match (eval_expr e2 env) with 
                                  InZ(0) -> InZ(0)
                                | InZ(1) -> InZ(1)
                                | _      -> failwith "erreur: e2 doit etre un bool ( 0 ou 1 )"
                      | _      -> failwith "erreur: e1 doit etre un bool ( 0 ou 1 )"

    | ASTOr(e1, e2)       -> 
                    match (eval_expr e1 env) with
                        InZ(1) -> InZ(1)
                      | InZ(0) -> 
                              match (eval_expr e2 env) with 
                                  InZ(1) -> InZ(1)
                                | InZ(0) -> InZ(0)
                                | _      -> failwith "erreur: e2 doit etre un bool ( 0 ou 1 )"
                      | _      -> failwith "erreur: e1 doit etre un bool ( 0 ou 1 )"

    | ASTApp(e1, exprs)   -> unit (* TODO *)
    | ASTAbs(args, e1)    -> unit (* TODO *)
and eval_exprs es env =



(*
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.prog Lexer.token lexbuf in
  eval ....
*)