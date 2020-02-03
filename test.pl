:- use_module(clearance).
:- use_module(init_knowledge_base).

:- initialization main.

main :-
    initialize_knowledge_base.

is_equal(X,Y) :- X, Y.

p(X) :- q(X), r(X).

q(a).
q(b).
r(b).

solve(true) :- !.
solve((A,B)) :- !, solve(A),solve(B).
solve(A) :- clause(A,B),solve(B).
solve(B) :- call(B).

