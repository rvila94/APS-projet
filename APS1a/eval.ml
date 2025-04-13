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
    

let rec extract_argp_name argp =
  match argp with
      Argp (s, t) -> s
    | ArgpVar (s, t) -> s
    
and extract_argsp_names argsp =
  match argsp with
      ASTArgp (argp) -> [extract_argp_name argp]
    | ASTArgsp (argp, ags) -> (extract_argp_name argp) :: extract_argsp_names ags

let rec check_env id env  : valeur=
  match env with
      []                -> failwith ("erreur: pas trouvé "^id^" dans l'environnement global")
    | (ident, value)::s -> if (String.equal id ident) then value else check_env id s

let rec add_env id val0 env =
  match env with
      []                ->  [(id, val0)]
    | (ident, value)::s ->  if (String.equal id ident) then (ident,val0)::s
                            else (ident,value)::(add_env id val0 s)

let rec eval_expr expr env mem = 
  match expr with 
      ASTNum(n)           -> InZ(n)
    | ASTId(s)            -> 
                    begin match check_env s env with
                      | InA addr -> check_mem addr mem
                      | v        -> v
                      (*| _        -> failwith ("erreur: pas trouvé "^s^" dans l'environnement global") *)
                    end
  
    | ASTIf(e1, e2, e3)   ->
                     begin match (eval_expr e1 env mem) with
                        InZ(1) -> (eval_expr e2 env mem)
                      | InZ(0) -> (eval_expr e3 env mem)
                      | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                     end

    | ASTAnd(e1, e2)      -> 
                    begin match (eval_expr e1 env mem) with
                        InZ(0) -> InZ(0)
                      | InZ(1) -> (
                              match (eval_expr e2 env mem) with 
                                  InZ(0) -> InZ(0)
                                | InZ(1) -> InZ(1)
                                | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                      )
                      | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                    end

    | ASTOr(e1, e2)       -> 
                    begin match (eval_expr e1 env mem) with
                        InZ(1) -> InZ(1)
                      | InZ(0) -> (
                              match (eval_expr e2 env mem) with 
                                  InZ(1) -> InZ(1)
                                | InZ(0) -> InZ(0)
                                | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                      )
                      | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
                    end

    | ASTApp(e, es)       -> 
                    let f = eval_expr e env mem in
                    let args = eval_exprs es env mem in
                    apply f args mem

    | ASTAbs(args, e)     ->  InF (e, extract_args_names args, env)

and eval_exprs exprs env mem =
  match exprs with
      ASTExpr(e)      -> [eval_expr e env mem]
    | ASTExprs(e, es) -> eval_expr e env mem :: eval_exprs es env mem
    
and eval_exprp exprp env mem = 
  match exprp with 
      ASTExpr(e) -> eval_expr e env mem
    | ASTAdr(s)  -> begin match (check_env s env) with
                  InA(addr) -> InA(addr)
                | v         -> v
               end
      
and eval_exprsp exprsp env mem =
  match exprsp with
      ASTExprp(e)      -> [eval_exprp e env mem]
    | ASTExprsp(e, es) -> eval_exprp e env mem :: eval_exprsp es env mem

and apply f args mem =
  match f with 
      InPrim prim -> prim args
    | InF (body, params, env)         ->
        let new_env = List.fold_left2 (fun env param arg -> add_env param arg env) env params args in   
        eval_expr body new_env mem

    | InFR (body, fname, params, env) ->
        let rec_env = add_env fname (InFR (body, fname, params, env)) env in
        let new_env = List.fold_left2 (fun env param arg -> add_env param arg env) rec_env params args in
        eval_expr body new_env mem

    | InP (cmds, params, env') ->
        let new_env = List.fold_left2 (fun e p a -> add_env p a e) env' params args in
        let _, _ = eval_cmds cmds new_env [] mem in
        InZ(0)

    | InPR (cmds, fname, params, env') ->
        let rec_env = add_env fname (InPR (cmds, fname, params, env')) env' in
        let new_env = List.fold_left2 (fun e p a -> add_env p a e) rec_env params args in
        let _, _ = eval_cmds cmds new_env [] mem in
        InZ(0)

    | _                               -> failwith "erreur: f n'est pas valide"

and apply_proc p args flux mem =
  match p with 
      InP (cmds, params, env') ->
      let new_env = List.fold_left2 (fun e p a -> add_env p a e) env' params args in
      eval_cmds cmds new_env flux mem

    | InPR (cmds, fname, params, env') ->
      let rec_env = add_env fname (InPR (cmds, fname, params, env')) env' in
      let new_env = List.fold_left2 (fun e p a -> add_env p a e) rec_env params args in
      eval_cmds cmds new_env flux mem

    | _                               -> failwith "erreur: p n'est pas valide"

and eval_stat s env flux mem =
  match s with
      ASTEcho(e)          ->
              begin match (eval_expr e env mem) with
                  InZ(n) -> (n :: flux, mem) 
                | _      -> failwith "erreur: mauvais types, doit etre un entier"
              end

    | ASTSet(x, e) ->
              begin match (check_env x env) with
                  InA(addr) -> 
                      let affectation = eval_expr e env mem in
                      let new_mem = modif_mem addr affectation mem in
                      flux, new_mem
                | _         -> failwith "erreur: adresse non valide"
              end
        

    | ASTIf2(e, bk1, bk2)   -> 
              begin match (eval_expr e env mem) with
                  InZ(1) -> (eval_block bk1 env flux mem)
                | InZ(0) -> (eval_block bk2 env flux mem)
                | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
              end

    | ASTWhile(e, b)      -> 
              begin match (eval_expr e env mem) with
                  InZ(1) ->  
                    let (new_flux, new_mem) = eval_block b env flux mem in
                    eval_stat s env new_flux new_mem
                | InZ(0) -> flux, mem
                | _      -> failwith "erreur: mauvais type, doit etre un bool ( 0 ou 1 )"
              end
    
    | ASTCall(fname, es)  -> 
              let p = check_env fname env in
              let args = eval_exprsp es env mem in
              apply_proc p args flux mem;


                  


and eval_def d env mem = 
  match d with
      ASTConst (x, _, e)               ->
                    let v = eval_expr e env mem in
                    (add_env x v env, mem)

    | ASTVar(x, _)                     -> 
                    let (addr, new_mem) = alloc mem in
                    let new_env = add_env x (InA addr) env in
                    new_env, new_mem

    | ASTFun (fname, _, args, body)    ->
                    let params = extract_args_names args in
                    add_env fname (InF(body, params, env)) env, mem

    | ASTFunRec (fname, _, args, body) ->
                    let params = extract_args_names args in
                    add_env fname (InFR(body, fname, params, env)) env, mem

    | ASTProc(x, args, ASTBlock(b))    ->
                    let params = extract_argsp_names args in
                    add_env x (InP(b, params, env)) env, mem

    | ASTProcRec(x, args, ASTBlock(b))  -> 
                    let params = extract_argsp_names args in
                    add_env x (InPR(b, x, params, env)) env, mem


and eval_cmds cmds env flux mem =
  match cmds with
    ASTStat(s)      -> eval_stat s env flux mem
  | ASTDef(d, cs)   -> 
                let new_env, new_mem = eval_def d env mem in
                eval_cmds cs new_env flux new_mem
  | ASTStat2(s, cs) -> 
                let (new_flux, new_mem) = (eval_stat s env flux mem) in
                eval_cmds cs env new_flux new_mem

and eval_block b env flux mem =
  match b with
      ASTBlock(cs) -> eval_cmds cs env flux mem

let eval_prog p =
  match p with
      ASTProg(b) -> 
              let flux, _ = (eval_block b initial_env [] []) in
              List.iter (function x -> Printf.printf "%d\n" x) (List.rev flux)
         
  



let _ =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.prog Lexer.token lexbuf in
  ( eval_prog p )

