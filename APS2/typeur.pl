extract_typeArgs([], []).
extract_typeArgs(ARGS, TS) :- 
    ARGS = [(_, T) | ARGS2],
    TS = [T | TS2],
    extract_typeArgs(ARGS2, TS2).

extract_typeArgsp([], []).
extract_typeArgsp([(_, T) | ARGS], [T | TS]) :-
    extract_typeArgsp(ARGS, TS).

extract_typeExprs(_, [], []).
extract_typeExprs(G, ES, TS) :- 
    ES = [E | ES2],
    TS = [T | TS2], 
    bt_expr(G, E, T),
    extract_typeExprs(G, ES2, TS2).

extract_typeExprsp(_, [], []).
extract_typeExprsp(G, [E | ES], [T | TS]) :-
    bt_exprp(G, E, T),
    extract_typeExprsp(G, ES, TS).

transform_args([], []).
transform_args([var(X, T) | Rest], [(X, ref(T)) | RestT]) :-
    transform_args(Rest, RestT).
transform_args([(X, T) | Rest], [(X, T) | RestT]) :-
    transform_args(Rest, RestT).

% Prog
bt_prog(prog(CS)) :-
    is_init_env(G),
    bt_block(G, CS).

is_init_env(G) :- 
    G = [ ("true", bool),
     ("false", bool),
     ("not", flech([bool],bool)),
     ("eq", flech([int, int], bool)),
     ("lt", flech([int, int], bool)),
     ("add", flech([int, int], int)),
     ("sub", flech([int, int], int)),
     ("mul", flech([int, int], int)),
     ("div", flech([int, int], int))].

% Bloc
bt_block(G, block(CS)) :-
    bt_cmds(G, CS).

% Defs
bt_cmds(G, [def(D) | CS]) :-
    bt_def(G, D, G2),
    bt_cmds(G2, CS).

% Stats
bt_cmds(G, [stat(S) | CS]) :-
    bt_stat(G, S),
    bt_cmds(G, CS).

% End
bt_cmds(G, [stat(S)]) :-
    bt_stat(G, S).

% Const
bt_def(G, const(X, T, E), [(X, T) | G]) :-  
    bt_expr(G, E, T).

% Fun
bt_def(G, fun(X, T, ARGS, E), Gfinal) :- 
    append(ARGS, G, G2),
    bt_expr(G2, E, T),
    Gfinal = [(X, Tflech) | G],
    Tflech = flech(TS, T),
    extract_typeArgs(ARGS, TS).

% FunRec
bt_def(G, funRec(X, T, ARGS, E), Gfinal) :- 
    extract_typeArgs(ARGS, TS),
    Tflech = flech(TS, T), 
    append([(X, Tflech)], ARGS, G2),
    append(G2, G, G3),
    bt_expr(G3, E, T),
    Gfinal = [(X, Tflech) | G].

% Var
bt_def(G, var(X, T), [(X, ref(T)) | G]) :-
    member(T, [int, bool]).

% Proc
bt_def(G, proc(X, Args, Block), Gfinal) :-
    transform_args(Args, Args2),
    extract_typeArgs(Args2, TS),
    append(Args2, G, G2),
    bt_block(G2, Block),
    Tflech = flech(TS, void),
    Gfinal = [(X, Tflech) | G].

% ProcRec
bt_def(G, procRec(X, Args, Block), Gfinal) :-
    transform_args(Args, Args2),
    extract_typeArgs(Args2, TS),
    Tflech = flech(TS, void),
    transform_args(Args, Args2),
    append([(X, Tflech) | Args2], G, G2),
    bt_block(G2, Block),
    Gfinal = [(X, Tflech) | G].

% Echo
bt_stat(G, echo(E)) :-
    bt_expr(G, E, int).

% Set
bt_stat(G, set(id(X), E)) :-
    member((X, ref(T)), G),
    bt_expr(G, E, T).

bt_stat(G, set(nth(E1, E2), E3)) :-
    bt_expr(G, E1, vec(T)),
    bt_expr(G, E2, int),
    bt_expr(G, E3, T).

bt_stat(G, set(X, E)) :-
    member((X, ref(T)), G),
    bt_expr(G, E, T).

% If2
bt_stat(G, if2(E, Bk1, Bk2)) :-
    bt_expr(G, E, bool),
    bt_block(G, Bk1),
    bt_block(G, Bk2).

% While
bt_stat(G, whilee(E, Bk)) :-
    bt_expr(G, E, bool),
    bt_block(G, Bk).

% Call
bt_stat(G, call(X, ES)) :-
    extract_typeExprsp(G, ES, TS0),
    member((X, flech(TSRef, void)), G),
    map_types_to_refs(TS0, TSRef).

map_types_to_refs([], []).
map_types_to_refs([T | TS], [ref(T) | TSRef]) :-
    map_types_to_refs(TS, TSRef).
map_types_to_refs([T | TS], [T | TSRef]) :-
    map_types_to_refs(TS, TSRef).
    
% Void
bt_expr(_, void, void).

% Num
bt_expr(_, num(N), int) :-
    % integer(N).
    number(N), 
    N =:= floor(N).

% Id
bt_expr(G, id(X), T) :-
    member((X,T), G ).

bt_expr(G, id(X), T) :-
    member((X, ref(T)), G).

% If
bt_expr(G, if(E1, E2, E3), T) :-
    bt_expr(G, E1, bool),
    bt_expr(G, E2, T),
    bt_expr(G, E3, T).

% And
bt_expr(G, and(E1, E2), bool) :-
    bt_expr(G, E1, bool),
    bt_expr(G, E2, bool).

% Or
bt_expr(G, or(E1, E2), bool) :-
    bt_expr(G, E1, bool),
    bt_expr(G, E2, bool).

% App
bt_expr(G, app(E, ES), T) :-
    Tflech = flech(TS, T),
    bt_expr(G, E, Tflech),
    extract_typeExprs(G, ES, TS).

% Abs
bt_expr(G, abs(ARGS, E), Tflech) :-
    append(ARGS, G, G2),
    bt_expr(G2, E, T),
    Tflech = flech(TS, T),
    extract_typeArgs(ARGS, TS).

% alloc
bt_expr(G,alloc(E),vec(_)) :- 
	bt_expr(G,E,int).

% Len
bt_expr(G,len(E),int) :- 
	bt_expr(G,E,vec(_)).

% Nth
bt_expr(G,nth(E1,E2),T) :-
	bt_expr(G,E1,vec(T)),
	bt_expr(G,E2,int).

% Vset
bt_expr(G, vset(E1, E2, E3), vec(T)) :-
    bt_expr(G,E1,vec(T)),
    bt_expr(G,E2,int),
    bt_expr(G,E3,T).

% Val
bt_exprp(G, X, T) :-
	bt_expr(G, X, T).

% Ref
bt_exprp(G, adr(X), ref(T)) :-
    member((X, ref(T)), G).

% main
:-
    read(P),
    % writeln(P),
    ( bt_prog(P) ->
        writeln('OK')
    ; writeln('Type Error')
    ),
    halt.

% exemple de commande: ./prologTerm ../Samples/prog01.aps | swipl typeur.pl
