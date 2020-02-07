:- module(clearance_api, [
    create_user_as_user/4,
    create_document_as_user/4,
    remove_user_as_user/2,
    remove_document_as_user/2,
    update_user_clearance_as_user/3,
    update_document_clearance_as_user/3
]).

:- use_module(library(lists)).
:- use_module(clearance_base).

create_user_as_user(User, Clearance, AccessUser, R) :-      has_user_rights(AccessUser, Clearance),
                                                            insert_user_with_clearance(User, Clearance, CreatedUser),
                                                            R = CreatedUser.
                                                            
create_document_as_user(Document, Clearance, AccessUser, R) :-  has_document_rights(AccessUser, Clearance),
                                                                insert_document_with_clearance(Document, Clearance, Document),
                                                                R = Document.

update_user_clearance_as_user(User, Clearance, AccessUser) :-   has_user_rights(AccessUser, Clearance),
                                                                remove_user_clearance(User),
                                                                insert_user_clearance(User, Clearance).                                           
                                                            
update_document_clearance_as_user(Document, Clearance, AccessUser) :-   has_document_rights(AccessUser, Clearance),
                                                                        remove_document_clearance(Document),
                                                                        insert_document_clearance(Document, Clearance).  

remove_user_as_user(User, AccessUser) :-    userClearance(User, UserClearance),
                                            has_user_rights(AccessUser, UserClearance),
                                            remove_user(User).

remove_document_as_user(Document, AccessUser) :-    documentClearance(Document, DocumentClearance),
                                                    has_document_rights(AccessUser, DocumentClearance),
                                                    remove_document(Document).

% ToDo nested interaction



% ToDo Api results
% can_read(User, Document, true) :- can_read(User, Document).
% can_read(_, _, false) :- !.
% remove_user_and_clearance(User, true) :- remove_user_and_clearance(User).
% remove_user_and_clearance(_, _, false) :- !.
% create_user_with_user(UserCreate, Clearance, UserAccess, true) :- create_user_with_user(UserCreate, Clearance, UserAccess).
% create_user_with_user(_, _, _, false) :- !.
% create_document_with_user(Document, Clearance, UserAccess, true) :- create_document_with_user(Document, Clearance, UserAccess).
% create_document_with_user(_, _, _, false) :- !.