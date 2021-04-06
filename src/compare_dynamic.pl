:- load_files(student, []).
:- load_files(model, []).
:- load_files(library(lists), []).

:- set_prolog_flag(toplevel_print_options,[quoted(true), portrayed(true), max_depth(0)]).

% comparing SLD to SLD, "main" of programme
compare(X) :-
    solve(student:X, SSLD),
    solve(model:X, MSLD),
    evaluate(SSLD, MSLD).


% evaluation of two SLD trees
evaluate([], []) :-
    format("Depth of submission's SLD tree is exactly the same as model solution's. Well done!\n", []).
evaluate([], _) :-
    format("Depth of submission's SLD tree is shallower than model solution's.\n", []),
    format("Predicates may be overspecified, consider making predicates more general.\n", []).
evaluate(_, []) :-
    format("Depth of submission's SLD tree is deeper than model solution's.\n", []),
    format("Consider optimising with in-built predicates or by reducing recursion.\n", []).
evaluate([S | Student], [M | Model]) :- % S and M are lists
    (S == M ->
      format('~w is correct.\n', [S]);
      format('~w is wrong.\n', [S])),
    (member(M, Student) -> format('~w is misplaced. \n', [M]); true),
    evaluate(Student, Model).


% helper to add Suffix to the end of all lists in Prefix
appendToAll([], _, []).
appendToAll([P | Prefix], Suffix, [C | Combined]) :-
    append(P, [Suffix], C),
    appendToAll(Prefix, Suffix, Combined).

% solve for SLD tree of goal
solve(_:true, []) :- !.
% solve(_:false, []) :- !.

% solve(M:(A,B), SLDOfAB) :-
%     solve(M:A, SLDofA),
%     last(SLDofA, [false]),
%     appendToAll(SLDofA, B, SLDOfAB).

% (S == M ->
%   format('~w is correct.\n', [S]);
%   format('~w is wrong.\n', [S]))

solve(M:(A,B), SLD) :- !,
    solve(M:A, SLDofA),
    appendToAll(SLDofA, B, SLDOfAB),
    solve(M:B, SLDofB),
    append(SLDOfAB, SLDofB, SLD).

solve(_:A, [[true]]) :- predicate_property(A,built_in), call(A).
solve(_:A, [[false]]) :- predicate_property(A,built_in), \+ call(A).

solve(_:A, [[true]]) :- predicate_property(A,autoload(_)), call(A).
solve(_:A, [[false]]) :- predicate_property(A,autoload(_)), \+ call(A).

solve(M:A, [[B] | SLD]) :- % SLD should be a list of lists
    clause(M:A,B),
    solve(M:B, SLD).
solve(M:A, [[false]]) :- \+ clause(M:A,_).

% solve(A,B,SLD) :-
%     solve(A, SLDofA),
%     appendToAll(SLDofA, B, SLDOfAB),
%     solve(B, SLDofB),
%     append(SLDOfAB, SLDofB, SLD).
