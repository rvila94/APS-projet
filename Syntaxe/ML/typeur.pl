bt_prog(prog(Cs)) :-
    bt_cmds(G, CS),
    is_init_env(G).

is_init_env(G) :- 
    G = [ ("add", fun_t([int_t, int_t]), int_t)].

bt_cmds(_,_).

bt_expr(G, id(x), T) :- member( (X,T), G ).

