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


r(a,b).
r(b,c).  r(c,d).

t(X,Y) :- r(X,Y).
t(X,Z) :- r(X,Y), t(Y,Z).

democount(true, _, 0) :- !.
democount((X,Y), P/N, K) :- !, 
    democount(X, P/N, I),
    democount(Y, P/N, J), 
    K is I + J. 

democount(X, P/N, K) :- 
    functor(X, P, N), !,
    clause(X, B), 
    democount(B, P/N, J),
    K is J + 1.

democount(X, P/N, J) :- 
    clause(X, B), 
    democount(B, P/N, J).

/*
?- democount(t(a,d), t/2, K).
> K = 3.

?- democount(t(a,V), t/2, 3).
> V = d.
*/