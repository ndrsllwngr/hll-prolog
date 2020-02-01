:- module(clearance, [canAccess/2]).

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

% User
user(timo).

userClearance(timo,secret).

%documents
document(nuclearCodes).
document(weirdPornMovies).

documentClearance(nuclearCodes,topsecret).
documentClearance(weirdPornMovies,restricted).
documentClearance(panamaPapers,secret).

% Rules
canAccess(U,D) :- userClearance(U,UL), documentClearance(D,DL), ((UL == DL) ; hierarchy(UL,DL)).

hierarchy(P,L) :-  superior(P, L).
hierarchy(P,L) :-  superior(P, X), hierarchy(X,L).

nextLevel(X,Y) :- superior(X,Y).

% -------update datamodel ------
% change clearance of document

% change clearance of user

% delete document

% insert document

% insert user