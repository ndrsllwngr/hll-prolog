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

% ------- Web Server --------
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% ------- Routes --------
% GET
:- http_handler('/health', health_request, []).
:- http_handler('/document', get_document_as_user_request, []).
:- http_handler('/document/get_accessible', get_documents_accessible_by_user_request, []).
:- http_handler('/user/get_managable', get_users_managable_by_user_request, []).

% POST
% User
:- http_handler('/user/create_as_user', create_user_as_user_request, []).
:- http_handler('/user/update_clearance_as_user', update_user_clearance_as_user_request, []).
:- http_handler('/user/remove_as_user', remove_user_as_user_request, []).
% Document
:- http_handler('/document/create_as_user', create_document_as_user_request, []).
:- http_handler('/document/update_clearance_as_user', update_document_clearance_as_user_request, []).
:- http_handler('/document/remove_as_user', remove_document_as_user_request, []).
:- http_handler('/document/grant_special_permission_as_user', grant_special_permission_as_user_request, []).
:- http_handler('/document/retract_special_permission_as_user', retract_special_permission_as_user_request, []).

% ------- Handle requests --------
% GET

% example: 
% GET http://localhost:5004/health
health_request(_) :-
    health(R),
    prolog_to_json(R, JSONOut),
    reply_json(JSONOut).

% example: 
% GET http://localhost:5004/document?document=nsa_files&access_user=director
get_document_as_user_request(Request) :-
    catch(
    http_parameters(Request,
       [
        document(Document, [optional(false)]),
        access_user(AccessUser, [optional(false)])
       ]),
    _E,
    fail),
    get_document(Document, AccessUser, R) ->
    reply_json(R);
    prolog_to_json(failure, JSONOut),
    reply_json(JSONOut).

% example: 
% GET http://localhost:5004/document/get_accessible?access_user=director
get_documents_accessible_by_user_request(Request) :-
    catch(
    http_parameters(Request,
       [
        access_user(AccessUser, [optional(false)])
       ]),
    _E,
    fail),
    get_documents_accesible_by_user(AccessUser, R),
    reply_json(R).

% example: 
% GET http://localhost:5004/user/get_managable?access_user=director
get_users_managable_by_user_request(Request) :-
    catch(
    http_parameters(Request,
       [
        access_user(AccessUser, [optional(false)])
       ]),
    _E,
    fail),
    get_users_managable_by_user(AccessUser, R),
    prolog_to_json(R, JSONOut),
    reply_json(JSONOut).

% POST
% Users

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
    create_user_as_user(User, Clearance, AccessUser, Result) -> 
    prolog_to_json(Result, JSONOut),
    reply_json(JSONOut);
    atom_string(R, "Could not create user"),
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
    update_user_clearance_as_user(User, Clearance, AccessUser) ->
    prolog_to_json(success, JSONOut),
    reply_json(JSONOut);
    prolog_to_json(failure, JSONOut),
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
    http_read_json_dict(Request, _{user: UserString, access_user: AccessUserString}),
    atom_string(User, UserString),
    atom_string(AccessUser, AccessUserString),
    remove_user_as_user(User, AccessUser) ->
    prolog_to_json(success, JSONOut),
    reply_json(JSONOut);
    prolog_to_json(failure, JSONOut),
    reply_json(JSONOut).


% Documents

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
    create_document_as_user(Document, Clearance, AccessUser, Result) -> 
    prolog_to_json(Result, JSONOut),
    reply_json(JSONOut);
    atom_string(R, "Could not create document"),
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
    update_document_clearance_as_user(Document, Clearance, AccessUser) ->
    prolog_to_json(success, JSONOut),
    reply_json(JSONOut);
    prolog_to_json(failure, JSONOut),
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
    atom_string(AccessUser, AccessUserString),
    remove_document_as_user(Document, AccessUser) ->
    prolog_to_json(success, JSONOut),
    reply_json(JSONOut);
    prolog_to_json(failure, JSONOut),
    reply_json(JSONOut).

% example: 
% POST http://localhost:5004/document/grant_special_permission_as_user
% Content-Type: application/json
% { 
%   "user" : "press",
% 	"document" : "nsa_files",
%   "access_user" : "snowden"
% }
grant_special_permission_as_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{user: UserString, document: DocumentString, access_user: AccessUserString}),
    atom_string(User, UserString),
    atom_string(Document, DocumentString),
    atom_string(AccessUser, AccessUserString),
    grant_special_permission_as_user(User, Document, AccessUser) ->
    prolog_to_json(success, JSONOut),
    reply_json(JSONOut);
    prolog_to_json(failure, JSONOut),
    reply_json(JSONOut).

% example: 
% POST http://localhost:5004/document/retract_special_permission_as_user
% Content-Type: application/json
% {
%   "user" : "press", 
% 	"document" : "nsa_files",
%   "access_user" : "snowden"
% }
retract_special_permission_as_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{user: UserString, document: DocumentString, access_user: AccessUserString}),
    atom_string(User, UserString),
    atom_string(Document, DocumentString),
    atom_string(AccessUser, AccessUserString),
    retract_special_permission_as_user(User, Document, AccessUser) ->
    prolog_to_json(success, JSONOut),
    reply_json(JSONOut);
    prolog_to_json(failure, JSONOut),
    reply_json(JSONOut).