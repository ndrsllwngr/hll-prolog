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

should_evaluate(Text, Term) :-  call(Term) -> write_statement(Text,success); write_statement(Text,failure).

should_not_evaluate(Text, Term) :-  call(Term) -> write_statement(Text,failure); write_statement(Text,success).

to(should_evaluate(Text, Term),ExpectedResult) :-   term_variables(Term,L),
                                                    last_element_of_list(L,R), 
                                                    call(Term),
                                                    ExpectedResult should_equal R -> write_statement(Text,success) ; write_statement(Text,failure).

write_statement(Text, success) :-  ansi_format([fg(green), bold], 'Success', []), write_text(Text).
write_statement(Text, failure) :-  ansi_format([fg(red), bold], 'Failure', []), write_text(Text).
write_text(Text) :-  write(": "), ansi_format([italic], '~w\n', [Text]).

last_element_of_list([X],R) :- R = X.
last_element_of_list([_|Tail],R) :- last(Tail,R).