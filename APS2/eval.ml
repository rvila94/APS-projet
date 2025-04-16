open Ast

type valeur =
    InZ  of int
  | InA  of adresse
  | InF  of expr * string list * environnement
  | InFR of expr * string * string list * environnement
  | InP  of cmds * string list * environnement
  | InPR of cmds * string * string list * environnement
  | InPrim of (valeur list -> valeur)
  | InB of adresse * int

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
  let true_ = InZ(1) in
  let false_ = InZ(0) in
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

  [("true", true_);
  ("false", false_);
  ("not", not);
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
    
    | ASTAlloc(e) ->
                    begin match eval_expr e env mem with
                      InZ(n) when n >= 0 ->
                        let rec alloc_n k mem acc =
                          if k = 0 then (acc, mem)
                          else
                            let (addr, new_mem) = alloc mem in
                            alloc_n (k-1) new_mem (addr :: acc) in
                            let (addresses, _) = alloc_n n mem [] in
                            let base_addr = List.hd (List.rev addresses) in
                            InB (base_addr, n)
                    | InZ(_) -> failwith "erreur: entier negatif"
                    | _      -> failwith "erreur: mauvais type"
                    end
    
    | ASTLen(e) ->
                    begin match eval_expr e env mem with
                      InB(_, taille) -> InZ(taille)
                    | _              -> failwith "erreur: mauvais type"
                    end
    
    | ASTNth(e1, e2) ->
                    begin match eval_expr e1 env mem, eval_expr e2 env mem with
                      InB(addr, s), InZ(i) ->
                        if i < 0 || i >= s then failwith "erreur: indice i hors du tableau"
                        else check_mem (addr + i) mem
                    | _, _ -> failwith "erreur: mauvais types"
                    end

    | ASTVset(e1, e2, e3) ->
                    begin match eval_expr e1 env mem, eval_expr e2 env mem with
                      InB(addr, s), InZ(i) ->
                        if i < 0 || i >= s then failwith "erreur: indice i hors du tableau"
                        else
                          let value = eval_expr e3 env mem in
                          let _ = modif_mem (addr + i) value mem in
                          value
                    | _, _ -> failwith "erreur: mauvais types"
                    end
                    
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
    
and eval_lval expr env mem =
  match expr with
    ASTLvalId(id) ->
         begin match check_env id env with
          | InA(addr) -> InA(addr)
          | InB(a, n) -> InB(a, n)
          | _ -> failwith "erreur: mauvais type"
         end
         
  | ASTLvalNth(lv, e) -> 
      begin match eval_lval lv env mem with
        InB(addr, n) -> (
          match eval_expr e env mem with
            InZ(x) -> (
              match check_mem (addr + x) mem with
                InB(a, n) -> InB(a, n)
              | _ -> InA(addr + x)
            )
          | _ -> failwith "erreur: mauvais type"
        )
      | _ -> failwith "erreur: mauvais type"
      end

and eval_stat s env flux mem =
  match s with
      ASTEcho(e)          ->
              begin match (eval_expr e env mem) with
                  InZ(n) -> (n :: flux, mem) 
                | _      -> failwith "erreur: mauvais types, doit etre un entier"
              end

    | ASTSet(x, e) ->
              begin match eval_expr e env mem with
                  InZ(n) -> 
                    begin match eval_lval x env mem with
                      InA(addr) ->
                        let new_mem = modif_mem addr (InZ n) mem in
                        (flux, new_mem)
                    | _ -> failwith "erreur: mauvais type"
                    end
                 | _ -> failwith "erreur: mauvais type"
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

