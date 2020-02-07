
:- module(init_knowledge_base, [initialize_knowledge_base/0]).

:- use_module(clearance_base).

initialize_knowledge_base() :-
    insert_user_with_clearance(director,topsecret, _),
    insert_user_with_clearance(putin,topsecret, _),

    insert_user_with_clearance(snowden, secret, _),

    insert_user_with_clearance(timo,unclassified, _),
    insert_user_with_clearance(andy,unclassified, _),


    insert_document_with_clearance(nuclearCodes,topsecret, _),
    insert_document_with_clearance(nsa_files, secret, _),
    insert_document_with_clearance(nsa,restricted, _),
    insert_document_with_clearance(panamaPapers,secret,_).
