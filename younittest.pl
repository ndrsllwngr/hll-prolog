:- op(500, xfx, should_evaluate).
:- op(500, xfx, should_equal).
:- op(500, xfx, should_not_equal).

:- use_module(clearance).

should_equal(X, Y) :- X = Y.
should_equal([H1|T1], [H2|T2]) :-
sort(0, @=<, [H1|T1], S1),
sort(0, @=<, [H2|T2], S2),
S1 == S2.

should_not_equal(X, Y) :- \+ should_equal(X, Y).

should_evaluate(Text, Term) :- write(Text), write(" - "), call(Term) -> write("works\n"); write("fails\n").

:- initialization main.

main :-
    "Comparing numbers" should_evaluate (
        clearance:canAccess(putin,nuclearCodes),
        clearance:canAccess(timo, weirdPorn)
    ),
    "Comparing unequal numbers" should_evaluate (2==1).