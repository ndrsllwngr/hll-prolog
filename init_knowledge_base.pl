
:- module(init_knowledge_base, [initialize_knowledge_base/0]).

:- use_module(clearance).

initialize_knowledge_base() :-
    insert_user_with_clearance(timo,secret, _),
    insert_user_with_clearance(putin,topsecret, _),
    insert_user_with_clearance(andy,confidencial, _),
    insert_user_with_clearance(david,unclassified, _),

    insert_document_with_clearance(nuclearCodes,topsecret, _),
    insert_document_with_clearance(weirdPorn,restricted, _),
    insert_document_with_clearance(panamaPapers,secret,_).
