:- module(api, [start_server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(clearance).

% Webserver
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Routes
:- http_handler('/', check_access_rights_request, []).
:- http_handler('/users', get_users_request, []).
:- http_handler('/documents_with_clearance', get_documents_accessible_with_clearance_request, []).
:- http_handler('/documents_for_user', get_documents_accessible_by_user_request, []).
:- http_handler('/create_user', create_user_request, []).
:- http_handler('/create_document', create_document_request, []).

% example: 
% GET http://localhost:5004/?user=timo&document=panamaPapers
check_access_rights_request(Request) :-
    catch(
    http_parameters(Request,
       [
        user(User, [optional(false)]),
        document(Document, [optional(false)])
       ]),
    _E,
    fail),
    can_access(User,Document,R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).


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
% GET http://localhost:5004/users/
get_users_request(Request) :-
    catch(
    http_parameters(Request,[]),
    _E,
    fail),
    get_users(R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).

% example: 
% POST http://localhost:5004/create_user/
create_user_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{user:User, clearance:Clearance}),
    atom_string(UserA,User),
    atom_string(ClearanceA,Clearance),
    insert_user_with_clearance(UserA,ClearanceA,R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).

% example: 
% POST http://localhost:5004/create_document/
create_document_request(Request) :-
    member(method(post), Request), !,
    http_read_json_dict(Request, _{document:Document, clearance:Clearance}),
    atom_string(DocumentA,Document),
    atom_string(ClearanceA,Clearance),
    insert_document_with_clearance(DocumentA,ClearanceA,R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).