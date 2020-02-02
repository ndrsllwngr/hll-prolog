:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/json_convert)).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(clearance).

:- http_handler('/', check_access_rights_request, []).
:- http_handler('/users/', get_users_request, []).
:- http_handler('/documents_with_clearance/', get_documents_accessible_with_clearance_request, []).
:- http_handler('/documents_for_user/', get_documents_accessible_by_user_request, []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).


% example: 
% http://localhost:5004/?user=timo&document=panamaPapers
check_access_rights_request(Request) :-
    catch(
    http_parameters(Request,
       [
        % default for a missing param
        user(User, [optional(false)]),
        % if bar param is missing Bar will be unbound
        document(Document, [optional(false)])
       ]),
    _E,
    fail),
    canAccess(User,Document,R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).


% example: 
% http://localhost:5004/documents_for_user/?user=timo
get_documents_accessible_by_user_request(Request) :-
    catch(
    http_parameters(Request,
       [
        % default for a missing param
        user(User, [optional(false)])
       ]),
    _E,
    fail),
    get_documents_accesible_by_user(User,R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).


% example: 
% http://localhost:5004/documents_with_clearance/?clearance=topsecret
get_documents_accessible_with_clearance_request(Request) :-
    catch(
    http_parameters(Request,
       [
        % default for a missing param
        clearance(Clearance, [optional(false)])
       ]),
    _E,
    fail),
    get_documents_accesible_with_clearance(Clearance,R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).

% example: 
% http://localhost:5004/users/
get_users_request(Request) :-
    catch(
    http_parameters(Request,[]),
    _E,
    fail),
    get_users(R),
    prolog_to_json(R,JSONOut),
    reply_json(JSONOut).

page_content(_Request) -->
	html(
	    [
	    h1('Oops!'),
	    p('Some parameter wasnt valid')
	    ]).


something(R) :- R is 5.
%canAccessRet(User,Document,R) :- R is canAccess(User,Document).