% This file contains predicates which generate test case inputs

:- use_module(library(random)).

% generates a list of predicates with inputs

% example input
% predicate_gen([], safe([A-B]), [list, list], [[[h], 1, 3], [[z], 1, 3]]).


% input_gen(-R, +[(arg_type, restrictions)])
% takes a list of tuples of argument types and their restrictions,
% then generates a list of possible inputs

% example: input_gen(I, [(int_range, [0,5]), (int_random, [70, 80, 90]), (char, [p, r, o]), (list, [[h], 1, 3])]).

% base case, no restrictions or arg types left
% result is an empty list to collect previous results
input_gen([], []).

% if arg type is int_range, generate random int within the range
% Restriction must be of form [L, U] where
%   the random int to be generated is between L and U (non-inclusive)
input_gen([R | Results], [(int_range, [L, U]) | Rest]) :-
  random(L, U, R),
  input_gen(Results, Rest).

% if arg type is int_random, generate random int from given inputs
% Restriction must be of form [int] where
%   the random int to be generated from the list
input_gen([Elem | Results], [(int_random, Inputs) | Rest]) :-
  random_member(Elem, Inputs),
  input_gen(Results, Rest).

% if arg type is char, generate random char from given inputs
% Restriction must be of form [char] where
%   the random char to be generated from the list
input_gen([Elem | Results], [(char, Inputs) | Rest]) :-
  random_member(Elem, Inputs),
  input_gen(Results, Rest).

% if arg type is a list, create random lists of all possible lengths
% Restriction must be of form [[Inputs], L, U] where
%   [Inputs] is a list of possible members of the random lists
%   The length of the random lists range from L to U
input_gen([L | Results], [(list, Restriction) | Rest]) :-
  fixed_length_lists_gen(L, Restriction),
  input_gen(Results, Rest).

% all possible length random list generator
% generates random lists of length L to U (inclusive)
fixed_length_lists_gen([], [_, U1, U]) :- U1 is U+1.
fixed_length_lists_gen([List | Lists], [Inputs, L, U]) :-
  fixed_size_list_gen(List, Inputs, L),
  L1 is L + 1,
  fixed_length_lists_gen(Lists, [Inputs, L1, U]).

% % random length list generator
% % generates N lists of length between L and U (non-inclusive)
% random_length_lists_gen([], _, _, _, 0).
% random_length_lists_gen([List | Lists], L, U, Inputs, N) :-
%   random(L, U, R),
%   fixed_size_list_gen(List, Inputs, R),
%   random_length_lists_gen(Lists, L, U, Inputs, N0),
%   N is N0 + 1.

% generate a random list of length L
fixed_size_list_gen([], _, 0).
fixed_size_list_gen([Elem | List], Inputs, L) :-
  random_member(Elem, Inputs),
  fixed_size_list_gen(List, Inputs, L0),
  L is L0 + 1.
