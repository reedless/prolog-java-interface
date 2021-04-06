:- module(student,
          [
          fact/1,
          safe/1,
          safe_state/1,
          safe_helper/1,
          goal/1,
          pick/3,
          add_new/3,
          visited/2,
          crossing/3,
          succeeds/1
          ]).

:- load_files(library(lists), []).

:- set_prolog_flag(toplevel_print_options,[quoted(true), portrayed(true), max_depth(0)]).

:- dynamic safe/1, safe_helper/1, safe_state/1, goal/1, pick/3, add_new/3.
:- dynamic visited/2, crossing/3, initial/1, allowed/1,  succeeds/1, journey/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%         276 Introduction to Prolog           %
%                                              %
%         Coursework (crossings)       %
%                                              %
%         Assessed Coursework                  %
%                                              %
%         MODEL SOLUTION (da04)                 %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ------------  (utilities) DO NOT EDIT

forall(P,Q) :- \+ (P, \+ Q).

% The following might be useful for testing.
% You may edit it if you want to adjust
% the layout. DO NOT USE it in your submitted code.

write_list(List) :-
  forall(member(X, List),
         (write('  '), write(X), nl)
        ).

print_history(H) :-
 reverse(H, RevH),
 print_hist_list(RevH).

print_hist_list([]).
print_hist_list([State|Rest]) :-
 print_state(State),
 print_hist_list(Rest).

print_state([b,NHs,NZs]-[SHs,SZs]) :-
 format('*~w-~w~t~20| ~w-~w~n', [NHs,NZs,SHs,SZs]).
print_state([NHs,NZs]-[b,SHs,SZs]) :-
 format(' ~w-~w~t~20|*~w-~w~n', [NHs,NZs,SHs,SZs]).

% solutions for testing


solution([[z,z],[z],[z,z],[z],[h,h],[h,z],[h,h],[z],[z,z],[h],[h,z]]).
solution([[z,z],[z],[z,z],[z],[h,h],[h,z],[h,h],[z],[z,z],[z],[z,z]]).
solution([[h,z],[h],[z,z],[z],[h,h],[h,z],[h,h],[z],[z,z],[h],[h,z]]).
solution([[h,z],[h],[z,z],[z],[h,h],[h,z],[h,h],[z],[z,z],[z],[z,z]]).


%% --------- END (utilities)

%% ------ Add your code to this file here.


/* Add code for Step 1 below this comment */
% safe(Bank)
%
% safe/1 holds when Bank is a given list of items (those left
% behind on a bank when a journey is made) that is safe.

safe([[], _]).
safe([H, Z]) :- safe_helper([H, Z]).
safe_helper([_, []]).
safe_helper([[h | H], [z | Z]]) :- safe_helper([H, Z]).

% safe([[], _]):-
%   !.
% safe([H, Z]):-
%   length(Z, X),
%   length(H, Y),
%   Y >= X.

/* Add code for Step 2 below this comment */
% safe_state(State)
%
% safe_state/1 holds when State represents a state
% in which both of the banks are safe.

safe_state([b|T]-L) :- safe_state(T-L).
safe_state(L-[b|T]) :- safe_state(L-T).
safe_state(N-S) :- safe(N), safe(S).

/* Add code for Step 3 below this comment */
% goal(State)
%
% goal/1 holds when (given, ground) State is a valid goal state

goal([[], []]-[b, [h,h,h], [z,z,z]]).

/* Add code for Step 4 below this comment */
% pick(State1, Items, State2)
%
% pick/3 holds when a given state State1 and a single (ground) human/zombie
% is or pair of (ground) humans/zombies in the list Items are picked
% from State1, and State2 is the state obtained when the elements
% in Items are removed from State1.

pick([[h | T], Z], [h], [T, Z]).
pick([H, [z | T]], [z], [H, T]).
pick([[h | T], [z | L]], [h, z], [T, L]).
pick([[h, h | T], Z], [h, h], [T, Z]).
pick([H, [z, z | L]], [z, z], [H, L]).

/* Add code for Step 5 below this comment */
% add_new(Items, AccState, State)
%
% add_new/3 holds when (given, ground) elements in the list Items
% are added to their matching lists in state State

add_new([], A, A).
add_new([h | T], [H, Z], L) :- add_new(T, [[h | H], Z], L).
add_new([z | T], [H, Z], L) :- add_new(T, [H, [z | Z]], L).

/* Add code for Step 6 below this comment */
% visited(State, Sequence)
%
% visited/2 holds when a given state State is equivalent to some
% member of a given Sequence

visited(State, History) :- member(State, History).

/* Add code for Step 7 below this comment */
% crossing(State, Move, Next)
%
% crossing/3 holds when Move represents one of the possible (not
% necessarily safe) river crossings that humans and zombies can
% make in (given) state CurrentState and NextState is the state
% that then results when Move is made

crossing([b | North]-South, Move, NewNorth-[b | NewSouth]) :-
  pick(North, Move, NewNorth),
  add_new(Move, South, NewSouth).
crossing(North-[b | South], Move, [b | NewNorth]-NewSouth) :-
  pick(South, Move, NewSouth),
  add_new(Move, North, NewNorth).

/* Add code for Step 8 below this comment */
% succeeds(Sequence)
%
% succeeds/1 holds for a sequence (a list of states) that starts
% with the initial state (all objects on the north bank) and
% terminates with all objects on the south bank;
% Where each step is the result of "safe" journeys and no states are
% repeated.
/*  Uncomment the following if you wish to skip Step 8.
     Else Add code for Step 8  below this comment */

initial([b,[h,h,h],[z,z,z]]-[[],[]]).

allowed([h]).
allowed([z]).
allowed([h,z]).
allowed([h,h]).
allowed([z,z]).

succeeds(Sequence) :- initial(I), journey(I, [I], Sequence).

journey([[], []]-[b,[h,h,h],[z,z,z]], _, []).
journey(I, Seen, [S | Seq]) :-
  allowed(S),
  crossing(I, S, Next),
  safe_state(Next),
  \+(visited(Next, Seen)),
  journey(Next, [Next | Seen], Seq).

/*

succeeds(Sequence) :- solution(Sequence).

*/
