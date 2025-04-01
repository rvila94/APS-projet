open Ast

type valeur =
    InZ  of int
  | InA  of adresse
  | InF  of expr * string list * environnement
  | InFR of expr * string * string list * environnement
  | InP  of cmds * string list * environnement
  | InPR of cmds * string * string list * environnement
  | InPrim of (valeur list -> valeur)

and environnement = (string * valeur) list
and adresse = int

type flux_sortie = int list

type memoire = (adresse * valeur) list

let alloc mem = 
  let new_addr = List.length mem in       (* Nouvelle adresse = taille de la memoire, incrémenté à chaque fois *)
  (new_addr, (new_addr, InZ(0)) :: mem)   (* Valeur par défaut à l'allocation est 0 *)

let rec modif_mem addr new_val mem =
  match mem with
  | [] -> failwith "erreur: adresse non trouvée"
  | (a, v)::s -> 
              if (a = addr) then 
                (a, new_val)::s 
              else 
                (a, v)::(modif_mem addr new_val s)

let rec check_mem addr mem =
  match mem with
  | [] -> failwith "erreur: adresse non trouvée"
  | (a, v)::s -> if a = addr then v else check_mem addr s


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
  let sub = InPrim (function
      [InZ n1; InZ n2] -> InZ (n1 - n2)
    | _ -> failwith "erreur: mauvais types, doivent etre des entiers"
  ) in
  let mul = InPrim (function
    | [InZ n1; InZ n2] -> InZ (n1 * n2)
    | _ -> failwith "erreur: mauvais types, doivent etre des entiers"
  ) in
  let div = InPrim (function
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


let rec extract_args_names args =
  match args with
      ASTArg (Arg (x, _)) -> [x]
    | ASTArgs (Arg (x, _), ags) -> x :: extract_args_names ags

let rec check_env id env  : valeur=
  match env with
      []                -> failwith ("erreur: pas trouvé "^id^" dans l'environnement global")
    | (ident, value)::s -> if (String.equal id ident) then value else check_env id s

let rec add_env id val0 env =
  match env with
      []                ->  [(id, val0)]
    | (ident, value)::s ->  if (String.equal id ident) then (ident,val0)::s
                            else (ident,value)::(add_env id val0 s)

let rec eval_expr expr env = 
  match expr with 
      ASTNum(n)           -> InZ(n)
    | ASTId(s)            -> (* TODO *)
    | ASTIf(e1, e2,e3)    ->
                     begin match (eval_expr e1 env) with
                        InZ(1) -> (eval_expr e2 env)
                      | InZ(0) -> (eval_expr e3 env)
                      | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                     end

    | ASTAnd(e1, e2)      -> 
                    begin match (eval_expr e1 env) with
                        InZ(0) -> InZ(0)
                      | InZ(1) -> (
                              match (eval_expr e2 env) with 
                                  InZ(0) -> InZ(0)
                                | InZ(1) -> InZ(1)
                                | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                      )
                      | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                    end

    | ASTOr(e1, e2)       -> 
                    begin match (eval_expr e1 env) with
                        InZ(1) -> InZ(1)
                      | InZ(0) -> (
                              match (eval_expr e2 env) with 
                                  InZ(1) -> InZ(1)
                                | InZ(0) -> InZ(0)
                                | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                      )
                      | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                    end

    | ASTApp(e, es)       -> 
                    let f = eval_expr e env in
                    let args = eval_exprs es env in
                    apply f args

    | ASTAbs(args, e)     ->  InF (e, extract_args_names args, env)

and eval_exprs exprs env =
  match exprs with
      ASTExpr(e)      -> [eval_expr e env]
    | ASTExprs(e, es) -> eval_expr e env :: eval_exprs es env

and apply f args =
  match f with 
      InPrim prim -> prim args
    | InF (body, params, env)         ->
        let new_env = List.fold_left2 (fun env param arg -> add_env param arg env) env params args in   
        eval_expr body new_env

    | InFR (body, fname, params, env) ->
        let rec_env = add_env fname (InFR (body, fname, params, env)) env in
        let new_env = List.fold_left2 (fun env param arg -> add_env param arg env) rec_env params args in
        eval_expr body new_env

    | InP (cmds, params, env)         -> (* TODO *)

    | InPR (cmds, fname, param, env)  -> (* TODO *)

    | _                               -> failwith "erreur: f n'est pas valide"


let eval_stat s env flux =
  match s with
      ASTEcho(e)          ->
              begin match (eval_expr e env) with
                  InZ(n) -> (n :: flux) 
                | _      -> failwith "erreur: mauvais types, doit etre un entier"
              end

    | ASTSet(x, e)        -> (* TODO *)

    | ASTIf2(e, b1, b2)   -> (* TODO *)

    | ASTWhile(e, b)      -> (* TODO *)
    
    | ASTCall(fname, es)  -> (* TODO *)


let rec eval_def d env = 
  match d with
      ASTConst (x, _, e)               ->
                    let v = eval_expr e env in
                    add_env x v env

    | ASTVar(x, _)                     -> 
                    let (addr, new_mem) = alloc mem in
                    let new_env = add_env x addr env in
                    new_env, new_mem

    | ASTFun (fname, _, args, body)    ->
                    let params = extract_args_names args in
                    add_env fname (InF(body, params, env)) env

    | ASTFunRec (fname, _, args, body) ->
        let params = extract_args_names args in
        add_env fname (InFR(body, fname, params, env)) env

    | ASTProc(x, args, b)              -> (* TODO *)

    | ASTProcRec(x, args, b)           -> (* TODO *)
    
and eval_cmds cmds env flux =
  match cmds with
      ASTStat(s)      -> eval_stat s env flux
    | ASTDef(d, cs)   -> 
                  let new_env = eval_def d env in
                  eval_cmds cs new_env flux
    | ASTStat2(s, cs) -> (* TODO *)

and eval_block b env flux =
  match b with
      ASTBlock(cs) -> eval_cmds cs env flux 

let eval_prog p =
  match p with
      ASTProg(b) -> 
              let flux = (eval_block b initial_env []) in
              List.iter (function x -> Printf.printf "%d\n" x) (List.rev flux)
         
  



let _ =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.prog Lexer.token lexbuf in
  ( eval_prog p )

