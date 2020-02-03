:- use_module(init_knowledge_base).
:- use_module(api).

:- initialization main.

main :-
    initialize_knowledge_base,
    start_server(5004).
