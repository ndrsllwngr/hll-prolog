:- module(clearance_base, [
    % Facts
    user/1, userClearance/2, document/1, documentClearance/2,
    clearanceLevel/1,
    % Predicates
    has_document_rights/2,
    specialPermission/2,
    has_user_rights/2,
    higher_or_equal/2,
    higher/2,
    get_all_higher_clearances/2,
    insert_document_with_clearance/3,
    insert_document/1,
    insert_document_clearance/2,
    remove_document_and_relations/1,
    remove_document/1,
    remove_document_clearance/1,
    insert_user_with_clearance/3,
    insert_user/1,
    insert_user_clearance/2,
    remove_user_and_relations/1,
    remove_user/1,
    remove_user_clearance/1,
    insert_special_permission/2,
    remove_special_permission/2
]).

:- use_module(library(lists)).

% Facts

clearanceLevel(topsecret).
clearanceLevel(secret).
clearanceLevel(confidential).
clearanceLevel(restricted).
clearanceLevel(official).
clearanceLevel(unclassified).

superior(topsecret,secret).
superior(secret,confidential).
superior(confidential,restricted).
superior(restricted,official).
superior(official,unclassified).

% ToDo: specialPermission(User, Document).

:- dynamic user/1, userClearance/2, document/1, documentClearance/2, specialPermission/2.

% ------- rules ------

% Create/modify/delete Documents up to my clearance level
has_document_rights(User, Clearance) :- userClearance(User, UserClearance), higher_or_equal(UserClearance, Clearance).
% Create/modify/delete users up to one level below own clearance. Special rights: Topsecret user can create topsecret users aswell
has_user_rights(User, Clearance) :- userClearance(User, UserClearance), (UserClearance = topsecret -> higher_or_equal(UserClearance, Clearance) ; higher(UserClearance, Clearance)).

higher_or_equal(C1, C2) :-  C1 = C2.
higher_or_equal(C1, C2) :-  higher(C1, C2).

higher(C1, C2) :-  superior(C1, C2).
higher(C1, C2) :-  superior(C1, X), higher(X, C2).

get_all_higher_clearances(Clearance,R) :- superior(C,Clearance) -> get_all_higher_clearances(C,R1), R = [C | R1]; R = [].
% next_higher_level(C1, C2) :- superior(C1, C2).

% ------- update datamodel basic ------

insert_document_with_clearance(Document, Clearance, R) :-   remove_document_and_relations(Document),
                                                            insert_document(Document), 
                                                            insert_document_clearance(Document, Clearance),
                                                            R = Document.

insert_document(Document) :- asserta(document(Document)).
insert_document_clearance(Document, Clearance) :- asserta(documentClearance(Document, Clearance)).

remove_document_and_relations(Document) :- remove_document(Document),
                                        remove_document_clearance(Document),
                                        remove_special_permission(_, Document).

remove_document(Document) :- retractall(document(Document)).
remove_document_clearance(Document) :- retractall(documentClearance(Document,_)).

insert_user_with_clearance(User, Clearance, R) :-   remove_user_and_relations(User),
                                                    insert_user(User), 
                                                    insert_user_clearance(User, Clearance),
                                                    R = User.

insert_user(User) :- asserta(user(User)).
insert_user_clearance(User, Clearance) :- asserta(userClearance(User, Clearance)).

remove_user_and_relations(User) :-     remove_user(User),
                                    remove_user_clearance(User),
                                    remove_special_permission(User, _).
                                
remove_user(User) :- retractall(user(User)).
remove_user_clearance(User) :- retractall(userClearance(User, _)).

insert_special_permission(User, Document) :- asserta(specialPermission(User, Document)).
remove_special_permission(User, Document) :- retractall(specialPermission(_, Document)). % TODO correct?