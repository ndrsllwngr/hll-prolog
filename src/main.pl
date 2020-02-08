:- use_module(database_util).
:- use_module(rest_api).

:- initialization main.

main :-
    init_database_with_example_data,
    start_server(5004).
