:- module(clearance_api, [
    health/1,
    get_documents_accesible_by_user/2,
    get_users_managable_by_user/2,
    create_user_as_user/4,
    create_document_as_user/4,
    remove_user_as_user/2,
    remove_document_as_user/2,
    update_user_clearance_as_user/3,
    update_document_clearance_as_user/3
]).

:- use_module(library(lists)).
:- use_module(clearance_base).

health(R) :- atom_string(R, "Prolog Clearance System (PCS) API is healthy").

get_documents_accesible_by_user(AccessUser, R) :- findall(Document, ((documentClearance(Document, DocumentClearance), has_document_rights(AccessUser, DocumentClearance)); specialPermission(AccessUser, Document)), R).
get_users_managable_by_user(AccessUser, R) :- findall(User, (userClearance(User, UserClearance), has_document_rights(AccessUser, UserClearance)), R).


create_user_as_user(User, Clearance, AccessUser, R) :-      has_user_rights(AccessUser, Clearance),
                                                            insert_user_with_clearance(User, Clearance, CreatedUser),
                                                            R = CreatedUser.
                                                            

update_user_clearance_as_user(User, Clearance, AccessUser) :-   has_user_rights(AccessUser, Clearance),
                                                                remove_user_clearance(User),
                                                                insert_user_clearance(User, Clearance).    

remove_user_as_user(User, AccessUser) :-    userClearance(User, UserClearance),
                                            has_user_rights(AccessUser, UserClearance),
                                            remove_user_and_relations(User).                                     

create_document_as_user(Document, Clearance, AccessUser, R) :-  has_document_rights(AccessUser, Clearance),
                                                                insert_document_with_clearance(Document, Clearance, CreatedDocument),
                                                                R = CreatedDocument.
                                                          
update_document_clearance_as_user(Document, Clearance, AccessUser) :-   has_document_rights(AccessUser, Clearance),
                                                                        remove_document_clearance(Document),
                                                                        insert_document_clearance(Document, Clearance).  

remove_document_as_user(Document, AccessUser) :-    (documentClearance(Document, DocumentClearance); specialPermission(AccessUser, Document)),
                                                    has_document_rights(AccessUser, DocumentClearance),
                                                    remove_document(Document).

grant_special_permission_as_user(User, Document, AccessUser) :- userClearance(User, UserClearance),
                                                                has_user_rights(AccessUser, UserClearance),
                                                                insert_special_permission(User, Document).

retract_special_pemission_as_user(User, Document, AccessUser) :-    userClearance(User, UserClearance),
                                                                    has_user_rights(AccessUser, UserClearance),
                                                                    remove_special_permission(User, Document).