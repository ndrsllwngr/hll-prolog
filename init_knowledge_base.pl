
:- module(init_knowledge_base, [initialize_knowledge_base/0]).

:- use_module(clearance).

initialize_knowledge_base() :-
    insert_user_with_clearance(timo,secret),
    insert_user_with_clearance(putin,topsecret),
    insert_user_with_clearance(andy,confidencial),
    insert_user_with_clearance(david,unclassified),

    insert_document_with_clearance(nuclearCodes,topsecret),
    insert_document_with_clearance(weirdPorn,restricted),
    insert_document_with_clearance(panamaPapers,secret).
