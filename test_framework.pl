:- module(test_framework, [
        op(200, xfx, should_equal),
        op(200, xfx, should_not_equal),
        op(300, xfx, should_evaluate),
        op(300, xfx, should_not_evaluate),
        op(400, xfx, to),
        should_equal/2,
        should_not_equal/2,
        should_evaluate/2,
        should_not_evaluate/2,
        to/2
]).

:- op(200, xfx, should_equal).
:- op(200, xfx, should_not_equal).
:- op(300, xfx, should_evaluate).
:- op(300, xfx, should_not_evaluate).
:- op(400, xfx, to).


should_equal(X, Y) :- X = Y.
should_equal([H1|T1], [H2|T2]) :-   sort(0, @=<, [H1|T1], S1),
                                    sort(0, @=<, [H2|T2], S2),
                                    S1 == S2.

should_not_equal(X, Y) :- \+ should_equal(X, Y).

should_evaluate(Text, Term) :-  write_start(Text), 
                                call(Term) -> write_success; write_failure, fail.

should_not_evaluate(Text, Term) :-  write_start(Text), 
                                    call(Term) -> write_failure, fail; write_success.

to(should_evaluate(Text, Term),ExpectedResult) :-   write_start(Text), 
                                                    term_variables(Term,L),
                                                    last_element_of_list(L,R), 
                                                    call(Term),
                                                    ExpectedResult should_equal R -> write_success ; write_failure, fail.

write_start(Text) :-  write(Text), write(" - ").
write_success :- write("succeeds\n").
write_failure :- write("fails\n").

last_element_of_list([X],R) :- R = X.
last_element_of_list([_|Tail],R) :- last(Tail,R).