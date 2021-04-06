:- module(model,
          [
          safe/1,
          safe_state/1,
          goal/1,
          pick/3,
          add_new/3,
          visited/2,
          crossing/3,
          succeeds/1
          ]).

:- load_files(library(lists), []).

:- set_prolog_flag(toplevel_print_options,[quoted(true), portrayed(true), max_depth(0)]).

:- dynamic safe/1, safe_state/1, goal/1, pick/3, add_new/3, visited/2.
:- dynamic crossing/3, succeeds/1, initial/1, journey/3.

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

safe([[], _]):-
  !.
safe([H, Z]):-
  length(Z, X),
  length(H, Y),
  Y >= X.

/* Add code for Step 2 below this comment */
% safe_state(State)
%
% safe_state/1 holds when State represents a state
% in which both of the banks are safe.

safe_state([b | NBank]-SBank):-
  safe(NBank),
  safe(SBank).
safe_state(NBank-[b | SBank]):-
  safe(NBank),
  safe(SBank).

/* Add code for Step 3 below this comment */
% goal(State)
%
% goal/1 holds when (given, ground) State is a valid goal state

goal([b]-[[h, h, h], [z, z, z]]).
goal(_-[b, [h, h, h], [z, z, z]]).

/* Add code for Step 4 below this comment */
% pick(State1, Items, State2)
%
% pick/3 holds when a given state State1 and a single (ground) human/zombie
% is or pair of (ground) humans/zombies in the list Items are picked
% from State1, and State2 is the state obtained when the elements
% in Items are removed from State1.

pick([[_ | Hs], Z], [h], [Hs, Z]).
pick([H, [_ | Zs]], [z], [H, Zs]).
pick([[_ | Hs], Z], [h , Item], [H1, Z1]):-
  pick([Hs, Z], [Item], [H1, Z1]).
pick([H, [z, z | Zs]], [z, z], [H, Zs]).

/* Add code for Step 5 below this comment */
% add_new(Items, AccState, State)
%
% add_new/3 holds when (given, ground) elements in the list Items
% are added to their matching lists in state State

add_new([], FinalBank, FinalBank).
add_new([h | Is], [H, Z], Bank):-
  append([h], H, Hs),
  add_new(Is, [Hs, Z], Bank).
add_new([z | Is], [H, Z], Bank):-
  append([z], Z, Zs),
  add_new(Is, [H, Zs], Bank).

/* Add code for Step 6 below this comment */
% visited(State, Sequence)
%
% visited/2 holds when a given state State is equivalent to some
% member of a given Sequence

visited(State, Sequence):-
  member(State, Sequence).

/* Add code for Step 7 below this comment */
% crossing(State, Move, Next)
%
% crossing/3 holds when Move represents one of the possible (not
% necessarily safe) river crossings that humans and zombies can
% make in (given) state CurrentState and NextState is the state
% that then results when Move is made

crossing([b | NBank]-SBank, Move, NBankNew-[b | SBankNew]):-
  pick(NBank, Move, NBankNew),
  add_new(Move, SBank, SBankNew).

crossing(NBank-[b | SBank], Move, [b | NBankNew]-SBankNew):-
  pick(SBank, Move, SBankNew),
  add_new(Move, NBank, NBankNew).

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

/*

succeeds(Sequence) :- solution(Sequence).

*/

succeeds(Sequence):-
  initial(I),
  journey(I, [], Sequence).

initial([b,[h,h,h],[z,z,z]]-[[],[]]).

journey(I, _, []):-
  goal(I),
  !.

journey(I, VisitedStates, [Move | Sequence]):-
  crossing(I, Move, NextState),
  safe_state(I),
  \+visited(I, VisitedStates),
  append([I], VisitedStates, L),
  journey(NextState, L, Sequence).
