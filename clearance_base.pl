:- module(clearance_base, [
    has_document_rights/2,
    has_user_rights/2,
    higher_or_equal/2,
    higher/2,
    get_all_documents/1,
    get_documents_of_clearance/2,
    get_documents_accesible_with_clearance/2,
    get_documents_accesible_by_user/2, 
    get_all_users/1,
    get_users_of_clearance_or_lower/2,
    get_users_of_clearance/2,
    insert_document_with_clearance/3,
    insert_document/1,
    insert_document_clearance/2,
    remove_document_and_clearance/1,
    remove_document/1,
    remove_document_clearance/1,
    insert_user_with_clearance/3,
    insert_user/1,
    insert_user_clearance/2,
    remove_user_and_clearance/1,
    remove_user/1,
    remove_user_clearance/1
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

% ToDo: specialPermission(User, Document).

:- dynamic user/1, userClearance/2, document/1, documentClearance/2.

% ------- rules ------

% Create/modify/delete Documents up to my clearance level
has_document_rights(User, Clearance) :- userClearance(User, UserClearance), higher_or_equal(UserClearance, Clearance).
% Create/modify/delete users up to one level below own clearance. Special rights: Topsecret user can create topsecret users aswell
has_user_rights(User, Clearance) :- userClearance(User, UserClearance), (UserClearance = topsecret -> higher_or_equal(UserClearance, Clearance) ; higher(UserClearance, Clearance)).

higher_or_equal(C1, C2) :-  C1 = C2.
higher_or_equal(C1, C2) :-  higher(C1, C2).

higher(C1, C2) :-  superior(C1, C2).
higher(C1, C2) :-  superior(C1, X), higher(X, C2).

% next_higher_level(C1, C2) :- superior(C1, C2).

% ------- get data ------

get_documents_accesible_by_user(User, R) :- findall(Document, (documentClearance(Document, DocumentClearance), has_document_rights(User, DocumentClearance)), R).

get_all_documents(R) :- findall(Document, document(Document), R).
get_documents_of_clearance(Clearance, R) :- findall(Document,(document(Document),documentClearance(Document,Clearance)),R).
get_documents_accesible_with_clearance(Clearance, R) :- findall(Document, (documentClearance(Document, DocumentClearance), higher_or_equal(Clearance, DocumentClearance)), R).

get_all_users(R) :- findall(U, user(U), R).
get_users_of_clearance_or_lower(Clearance, R) :- findall(User, (userClearance(User, UserClearance), higher_or_equal(Clearance, UserClearance)), R).
get_users_of_clearance(Clearance, R) :- findall(User, (user(User), userClearance(User, Clearance)), R).

% ------- update datamodel basic ------

insert_document_with_clearance(Document, Clearance, R) :-   remove_document_and_clearance(Document),
                                                            insert_document(Document), 
                                                            insert_document_clearance(Document, Clearance),
                                                            R = Document.

insert_document(Document) :- assert(document(Document)).
insert_document_clearance(Document, Clearance) :- assert(documentClearance(Document, Clearance)).

remove_document_and_clearance(Document) :-  remove_document(Document),
                                        remove_document_clearance(Document).

remove_document(Document) :- retractall(document(Document)).
remove_document_clearance(Document) :- retractall(documentClearance(Document,_)).

insert_user_with_clearance(User, Clearance, R) :-   remove_user_and_clearance(User),
                                                    insert_user(User), 
                                                    insert_user_clearance(User, Clearance),
                                                    R = User.

insert_user(User) :- assert(user(User)).
insert_user_clearance(User, Clearance) :- assert(userClearance(User, Clearance)).

remove_user_and_clearance(User) :-  remove_user(User),
                                    remove_user_clearance(User).
                                
remove_user(User) :- retractall(user(User)).
remove_user_clearance(User) :- retractall(userClearance(User, _)).