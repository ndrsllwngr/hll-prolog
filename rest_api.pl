:- module(rest_api, [start_server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(clearance_api).

% Webserver
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Routes
% GET
% User
:- http_handler('/users', get_users_request, []).
% Document
:- http_handler('/', check_access_rights_request, []).
:- http_handler('/documents_with_clearance', get_documents_accessible_with_clearance_request, []).
:- http_handler('/documents_for_user', get_documents_accessible_by_user_request, []).

% POST
% User
:- http_handler('/user/create_as_user', create_user_as_user_request, []).
:- http_handler('/user/update_clearance_as_user', update_user_clearance_as_user_request, []).
:- http_handler('/user/remove_as_user', remove_user_as_user_request, []).
% Document
:- http_handler('/document/create_as_user', create_document_as_user_request, []).
:- http_handler('/document/update_clearance_as_user', update_document_clearance_as_user_request, []).
:- http_handler('/document/remove_as_user', remove_document_as_user_request, []).


% example: 
% GET http://localhost:5004/?user=timo&document=panamaPapers
% check_access_rights_request(Request) :-
%     catch(
%     http_parameters(Request,
%        [
%         user(User, [optional(false)]),
%         document(Document, [optional(false)])
%        ]),
%     _E,
%     fail),
%     can_read(User,Document,R),
%     prolog_to_json(R,JSONOut),
%     reply_json(JSONOut).


% example: 
% GET http://localhost:5004/documents_for_user?user=timo
get_documents_accessible_by_user_request(Request) :-
    catch(
    http_parameters(Request,
       [
        user(User, [optional(false)])
       ]),
    _E,
    fail),
    get_documents_accesible_by_user(User,R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).


% example: 
% GET http://localhost:5004/documents_with_clearance?clearance=topsecret
get_documents_accessible_with_clearance_request(Request) :-
    catch(
    http_parameters(Request,
       [
        clearance(Clearance, [optional(false)])
       ]),
    _E,
    fail),
    get_documents_accesible_with_clearance(Clearance,R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).

% example: 
% GET http://localhost:5004/users
get_users_request(Request) :-
    catch(
    http_parameters(Request,[]),
    _E,
    fail),
    get_all_users(R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).

% -----
% USERS
% -----

% example: 
% POST http://localhost:5004/user/create_as_user
% Content-Type: application/json
% {
% 	"user" : "snowden",
% 	"clearance" : "restricted",
%   "access_user" : "director"
% }
create_user_as_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{user: UserString, clearance: ClearanceString, access_user: AccessUserString}),
    atom_string(User, UserString),
    atom_string(Clearance, ClearanceString),
    atom_string(AccessUser, AccessUserString),
    create_user_as_user(User, Clearance, AccessUser, R),  % ToDo handle false case
    prolog_to_json(R, JSONOut),
    reply_json(JSONOut).

% example: 
% POST http://localhost:5004/user/update_clearance_as_user
% Content-Type: application/json
% {
% 	"user" : "snowden",
% 	"clearance" : "secret",
%   "access_user" : "director"
% }
update_user_clearance_as_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{user: UserString, clearance: ClearanceString, access_user: AccessUserString}),
    atom_string(User, UserString),
    atom_string(Clearance, ClearanceString),
    atom_string(AccessUser, AccessUserString),
    update_user_clearance_as_user(User, Clearance, AccessUser), % ToDo handle result
    prolog_to_json(R, JSONOut),
    reply_json(JSONOut).

% example: 
% POST http://localhost:5004/user/remove_as_user
% Content-Type: application/json
% {
% 	"user" : "snowden",
%   "access_user" : "director"
% }
remove_user_as_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{user: DocumentString, access_user: AccessUserString}),
    atom_string(User, UserString),
    atom_string(AccessUser, AccessUserString),
    remove_user_as_user(Document, AccessUser), % ToDo handle result
    prolog_to_json(R, JSONOut),
    reply_json(JSONOut).

% ---------
% DOCUMENTS
% ---------

% example: 
% POST http://localhost:5004/document/create_as_user
% Content-Type: application/json
% {
% 	"document" : "nsa_files",
% 	"clearance" : "secret",
%   "access_user" : "snowden"
% }
create_document_as_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{document: DocumentString, clearance: ClearanceString, access_user: AccessUserString}),
    atom_string(Document, DocumentString),
    atom_string(Clearance, ClearanceString),
    atom_string(AccessUser, AccessUserString),
    create_document_as_user(Document, Clearance, AccessUser, R), % ToDo handle false case
    prolog_to_json(R, JSONOut),
    reply_json(JSONOut).

% example: 
% POST http://localhost:5004/document/update_clearance_as_user
% Content-Type: application/json
% {
% 	"document" : "nsa_files",
% 	"clearance" : "secret",
%   "access_user" : "snowden"
% }
update_document_clearance_as_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{document: DocumentString, clearance: ClearanceString, access_user: AccessUserString}),
    atom_string(Document, DocumentString),
    atom_string(Clearance, ClearanceString),
    atom_string(AccessUser, AccessUserString),
    update_document_clearance_as_user(Document, Clearance, AccessUser), % ToDo handle result
    prolog_to_json(R, JSONOut),
    reply_json(JSONOut).

% example: 
% POST http://localhost:5004/document/remove_as_user
% Content-Type: application/json
% {
% 	"document" : "nsa_files",
%   "access_user" : "snowden"
% }
remove_document_as_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{document: DocumentString, access_user: AccessUserString}),
    atom_string(Document, DocumentString),
    atom_string(Clearance, ClearanceString),
    atom_string(AccessUser, AccessUserString),
    remove_document_as_user(Document, AccessUser), % ToDo handle result
    prolog_to_json(R, JSONOut),
    reply_json(JSONOut).