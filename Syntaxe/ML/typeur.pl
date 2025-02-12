extract_typeArgs([], []).
extract_typeArgs(ARGS, TS) :- 
    ARGS = [(_, T) | ARGS2],
    TS = [T | TS2],
    extract_typeArgs(ARGS2, TS2).

extract_typeExprs(_, [], []).
extract_typeExprs(G, ES, TS) :- 
    ES = [E | ES2],
    TS = [T | TS2], 
    bt_expr(G, E, T),
    extract_typeExprs(G, ES2, TS2).



% Prog
bt_prog(prog(CS)) :-
    is_init_env(G),
    bt_cmds(G, CS).

is_init_env(G) :- 
    G = [ (true, bool),
     (false, bool),
     (not, flech([bool],bool)),
     (eq, flech([int, int], bool)),
     (lt, flech([int, int], bool)),
     (add, flech([int, int], int)),
     (sub, flech([int, int], int)),
     (mul, flech([int, int], int)),
     (div, flech([int, int], int))].

% Defs
bt_cmds(G, [def(D) | CS]) :-
    bt_def(G, D, G2),
    bt_cmds(G2, CS).

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
    append(ARGS, G, G2),
    bt_expr(G3, E, T),
    Gfinal = [(X, Tflech) | G],
    Tflech = flech(TS, T),
    extract_typeArgs(ARGS, TS),
    G3 = [(X, Tflech) | G2].    % tester de remlacer ca par append((X, Tflech), G2, G3)

% Echo
bt_stat(G, echo(E)) :-
    bt_expr(G, E, int).

% Num
bt_expr(_, num(_), int).

% Id
bt_expr(G, id(X), T) :-
    member((X,T), G ).

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

% main
:- 
    read(P),
    bt_prog(P).


% exemple de commande: ./prologTerm ../Samples/prog0.aps | swipl typeur.pl
% exemple commande dans prolog: bt_prog(prog([stat(echo(num(42)))])).