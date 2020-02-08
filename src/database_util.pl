
:- module(database_util, [
        init_database_with_example_data/0,
        clean_database/0,
        list_database_state/0
    ]).

:- use_module(clearance_base).

init_database_with_example_data :-
    insert_user_with_clearance(director,topsecret, _),
    insert_user_with_clearance(putin,topsecret, _),

    insert_user_with_clearance(snowden, secret, _),

    insert_user_with_clearance(timo,unclassified, _),
    insert_user_with_clearance(andy,unclassified, _),


    insert_document_with_clearance(nuclearCodes,topsecret, _),
    insert_document_with_clearance(nsa_files, secret, _),
    insert_document_with_clearance(nsa,restricted, _),
    insert_document_with_clearance(panamaPapers,secret,_),
    list_database_state.

clean_database :-
    retractall(document(_)),
    retractall(user(_)),
    retractall(documentClerance(_,_)),
    retractall(userClearance(_,_)),
    retractall(specialPermission(_,_)).

list_database_state :-
    write("\n---\nCurrent state of database:\n---\n\n"),
    listing(clearanceLevel),
    listing(document),
    listing(user),
    listing(documentClearance),
    listing(userClearance),
    listing(specialPermission).
    