bt_prog(prog(Cs)) :-
    bt_cmds(G, CS),
    is_init_env(G).

is_init_env(G) :- 
    G = [ ("true", bool)],
    G = [ ("false", bool)],
    G = [ ("not", astflech([bool]),bool)],
    G = [ ("eq", astflech([int, int]), bool)],
    G = [ ("lt", astflech([int, int]), bool)],
    G = [ ("add", astflech([int, int]), int)],
    G = [ ("sub", astflech([int, int]), int)],
    G = [ ("mul", astflech([int, int]), int)],
    G = [ ("div", astflech([int, int]), int)].

bt_cmds(_,_).

bt_expr(G, id(x), T) :- member( (X,T), G ).

