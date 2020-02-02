:- module(clearance, [canAccess/3,insert_user_with_clearance/2, insert_document_with_clearance/2, get_users/1, get_users_of_clearance_or_lower/2, get_documents_accesible_with_clearance/2, get_documents_accesible_by_user/2]).

:- use_module(library(lists)).

% Facts

clearanceLevel(topsecret).
clearanceLevel(secret).
clearanceLevel(confidencial).
clearanceLevel(restricted).
clearanceLevel(official).
clearanceLevel(unclassified).

superior(topsecret,secret).
superior(secret,confidencial).
superior(confidencial,restricted).
superior(restricted,official).
superior(official,unclassified).

:- dynamic user/1.
:- dynamic userClearance/2.

:- dynamic document/2.
:- dynamic documentClearance/2.

% User
user(timo).
user(putin).
user(andy).
user(david).

userClearance(timo,secret).
userClearance(putin,topsecret).
userClearance(andy,confidencial).
userClearance(david,unclassified).

%documents
document(nuclearCodes).
document(weirdPorn).

documentClearance(nuclearCodes,topsecret).
documentClearance(weirdPorn,restricted).
documentClearance(panamaPapers,secret).
 
% ------- rules ------

canAccess(U,D,R) :- canAccess(U,D) -> R = true; R = false.
canAccess(U,D) :- userClearance(U,CU), documentClearance(D,CD), higher_or_equal(CU,CD).

accessible_with_clearance(D,C) :- documentClearance(D,CD), higher_or_equal(C,CD).


higher_or_equal(P,L) :-  P == L.
higher_or_equal(P,L) :-  higher(P, L).

higher(P,L) :-  superior(P, L).
higher(P,L) :-  superior(P, X), higher(X,L).

next_higher_level(X,Y) :- superior(X,Y).

% ------- get data ------

get_documents_accesible_by_user(U,R) :-  findall(D, canAccess(U,D), R).
get_documents_accesible_with_clearance(C,R) :-  findall(D, (documentClearance(D,CD), higher_or_equal(C,CD)), R).
get_documents_of_clearance(C,R) :- findall(D,(document(D),documentClearance(D,C)),R).
get_documents(R) :- findall(D,document(D),R).

get_users_of_clearance_or_lower(C,R) :- findall(U,(userClearance(U,CU), higher_or_equal(C,CU)),R).
get_users_of_clearance(C,R) :- findall(U,(user(U),userClearance(U,C)),R).
get_users(R) :- findall(U,user(U),R).

% ------- update datamodel ------

insert_document(U) :- assert(document(U)).
insert_document_clearance(U,C) :- assert(documentClearance(U,C)).
insert_document_with_clearance(U,C) :- remove_document_and_clearance(U), insert_document(U), insert_document_clearance(U,C).

remove_document_and_clearance(U) :- remove_document(U), remove_document_clearance(U).
remove_document(U) :- retractall(document(U)).
remove_document_clearance(U) :- retractall(documentClearance(U,_)).

insert_user(U) :- assert(user(U)).
insert_user_clearance(U,C) :- assert(userClearance(U,C)).
insert_user_with_clearance(U,C) :- remove_user_and_clearance(U), insert_user(U), insert_user_clearance(U,C).

remove_user_and_clearance(U) :- remove_user(U), remove_user_clearance(U).
remove_user(U) :- retractall(user(U)).
remove_user_clearance(U) :- retractall(userClearance(U,_)).
