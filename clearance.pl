:- module(clearance, [
    %exported for test interpreter (solve)
    canAccess/2, user/1, userClearance/2, document/1, documentClearance/2, higher_or_equal/2, higher/2,
    canAccess/3,
    insert_user_with_clearance/3, 
    insert_document_with_clearance/3,
    get_users/1, 
    get_users_of_clearance_or_lower/2, 
    get_documents_accesible_with_clearance/2, 
    get_documents_accesible_by_user/2,
    %todo split into multiple modules
    insert_user_with_clearance/2, 
    insert_document_with_clearance/2
]).

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

:- dynamic user/1, userClearance/2, document/1, documentClearance/2.

% ------- rules ------

canAccess(U,D,true) :- canAccess(U,D).
canAccess(_,_,false) :- !.

canAccess(U,D) :- userClearance(U,CU), documentClearance(D,CD), higher_or_equal(CU,CD).

accessible_with_clearance(D,C) :- documentClearance(D,CD), higher_or_equal(C,CD).

higher_or_equal(P,L) :-  P = L.
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

% ------- update datamodel basic ------

insert_document(Document) :- assert(document(Document)).
insert_document_clearance(Document,Clearance) :- assert(documentClearance(Document,Clearance)).
insert_document_with_clearance(Document,Clearance) :- remove_document_and_clearance(Document), insert_document(Document), insert_document_clearance(Document,Clearance).

insert_document_with_clearance(Document,Clearance, true) :- insert_document_with_clearance(Document,Clearance).
insert_document_with_clearance(_,_, false) :- !.

remove_document_and_clearance(User) :- remove_document(User), remove_document_clearance(User).
remove_document(User) :- retractall(document(User)).
remove_document_clearance(User) :- retractall(documentClearance(User,_)).

insert_user(User) :- assert(user(User)).
insert_user_clearance(User,Clearance) :- assert(userClearance(User,Clearance)).
insert_user_with_clearance(User,Clearance) :- remove_user_and_clearance(User), insert_user(User), insert_user_clearance(User,Clearance).

insert_user_with_clearance(User,Clearance,true) :- insert_user_with_clearance(User,Clearance).
insert_user_with_clearance(_,_,false) :- !.

remove_user_and_clearance(User) :- remove_user(User), remove_user_clearance(User).
remove_user(User) :- retractall(user(User)).
remove_user_clearance(User) :- retractall(userClearance(User,_)).

% ------- update datamodel ------

create_user_with_user(UserCreate, Clearance, UserAccess) :- userClearance(UserAccess, UserAccessClearance), higher(UserAccessClearance, Clearance), insert_user_with_clearance(UserCreate,Clearance).

create_user_with_user(UserCreate, Clearance, UserAccess, true) :- create_user_with_user(UserCreate, Clearance, UserAccess).
create_user_with_user(_, _, _, false) :- !.

create_document_with_user(Document, Clearance, UserAccess) :- userClearance(UserAccess, UserAccessClearance), higher(UserAccessClearance, Clearance),insert_document_with_clearance(Document,Clearance).

create_document_with_user(Document, Clearance, UserAccess, true) :- create_document_with_user(Document, Clearance, UserAccess).
create_document_with_user(_, _, _, false) :- !.