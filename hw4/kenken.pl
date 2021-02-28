%% -*- mode: prolog; -*-

%%%%%%%%%%%%%% PART I: kenken/3 %%%%%%%%%%%%%%
% Helper functions from clpfd library for transpose matrix
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

length_flipped(N, L) :-
    length(L, N).

% set the domain
domain(N, L) :-
    fd_domain(L, 1, N).

% get the value given coordinate I|J andhuo store to V.
get_val([I|J], T, V) :-
    nth(I, T, R), 
    nth(J, R, V).

check_constraint(_, +(0, [])).
check_constraint(T, +(S, [Head|Tail])) :-
    get_val(Head, T, N),
    S #= N + Sum,
    check_constraint(T, +(Sum, Tail)).    % recursion

check_constraint(_, *(1, [])).
check_constraint(T, *(P, [Head|Tail])) :-
    get_val(Head, T, N),
    P #= N * Prod,
    check_constraint(T, *(Prod, Tail)).     % recursion

check_constraint(T, -(D, J, K)) :-
    get_val(J, T, M),
    get_val(K, T, N),
    (D + M #= N; D + N #= M).       % Or

check_constraint(T, /(Q, J, K)) :-
    get_val(J, T, M),
    get_val(K, T, N),
    (Q * M #= N; Q * N #= M).       % Or

%% Implementation of Kenken:
kenken(N, C, T) :-
    length(T, N),   % check row number=N
    maplist(length_flipped(N), T),  % check each row has length N
    maplist(domain(N), T),  % domain is from 1 to N
    maplist(check_constraint(T), C),
    maplist(fd_all_different, T),   % check every element is unique
    transpose(T, T2),
    maplist(fd_all_different, T2),
    maplist(fd_labeling, T).

kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).


%%%%%%%%%%%%%% PART II: Plain_kenken/3 %%%%%%%%%%%%%%

% replace fd_all_different
check_unique([]).
check_unique([Head|Tail]) :-
    \+(member(Head, Tail)), check_unique(Tail).

plain_list(N, L) :-
    findall(Num, between(1, N, Num), L).

% Use recursion to check, and store intermediate value at Prev
plain_add(_, Res, [], Res).
plain_add(T, Res, [Head|Tail], Prev) :-
    get_val(Head, T, V),
    Prev2 is Prev + V,
    plain_add(T, Res, Tail, Prev2).

plain_mult(_, Res, [], Res).
plain_mult(T, Res, [Head|Tail], Prev) :-
    get_val(Head, T, V),
    Prev2 is Prev * V,
    plain_mult(T, Res, Tail, Prev2).

plain_sub(_, Res, _, _, Res).
plain_sub(T, Res, J, K) :-
    get_val(J, T, A),
    get_val(K, T, B),
    (Prev is  A - B; Prev is B - A),
    plain_sub(T, Res, J, K, Prev).

plain_div(_, Res, _, _, Res).
plain_div(T, Res, J, K) :-
    get_val(J, T, A),
    get_val(K, T, B),
    (Prev is  A / B; Prev is B / A),
    plain_div(T, Res, J, K, Prev).

% def of this relation must all be put together
plain_check_constraint(T, +(Res, L)) :-
    plain_add(T, Res, L, 0).
plain_check_constraint(T, *(Res, L)) :-
    plain_mult(T, Res, L, 1).
plain_check_constraint(T, -(Res, J, K)) :-
    plain_sub(T, Res, J, K).
plain_check_constraint(T, /(Res, J, K)) :-
    plain_div(T, Res, J, K).

plain_kenken(N, C, T) :-
    length(T, N),
    maplist(length_flipped(N), T),
    plain_list(N, L),      % L is list [1..N]
    maplist(permutation(L), T),     % every row is a permutation of the newlist
    maplist(check_unique, T),
    transpose(T, T2),
    maplist(check_unique, T2),
    maplist(plain_check_constraint(T), C).


%%%%%%%%%%%%%% PART II-2: Efficiency Test %%%%%%%%%%%%%%
%% The test case is the 4*4 kenken matrix:
kenken_testcase_4(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ]
).

%% Measure the performance of the two implementations by statistics/0.
%% statistics, kenken_testcase_4(N,C), kenken(N,C,T), statistics.
%% statistics, kenken_testcase_4(N,C), plain_kenken(N,C,T), statistics.
%% The general performance is as followed:
%kenken:
%   user   time       0.001 sec
%   system time       0.001 sec
%   cpu    time       0.001 sec
%   real   time       0.002 sec
%
%plain_kenken:
%   user   time       0.498 sec
%   system time       0.001 sec
%   cpu    time       0.499 sec
%   real   time       0.500 sec

%% We can see that the plain_kenken takes over 100x more time than kenken,
%% so we can say that the plain_kenken has much worse performance.


%%%%%%%%%%%%%% PART III: noop_kenken %%%%%%%%%%%%%%
% noop_kenken/4 should have 4 arguments:
% noop_kenken(N, C, C2, T)
%     N: a nonnegative integer specifying the number of cells on each side of
%        the KenKen square.
%     C: a list of numeric cage constraints, for each constraint is (T, L) where
%        T is the target number and L is a list of coordinates [I|J] indicating
%        the row and col.
%     C2: list of numeric cage constraints, each with operator inserted,
%         in the form of:
%         +(S, L), *(P, L), -(D, J, K), (Q, J, K).
%     T: a list of list of integers. All the lists have length N. This represents
%        the N×N grid.

noop_kenken_testcase(
  6,
  [
   (11, [[1|1], [2|1]]),
   (2, [1|2], [1|3]),
   (20, [[1|4], [2|4]]),
   (6, [[1|5], [1|6], [2|6], [3|6]]),
   (3, [2|2], [2|3]),
   (3, [2|5], [3|5]),
   (240, [[3|1], [3|2], [4|1], [4|2]]),
   (6, [[3|3], [3|4]]),
   (6, [[4|3], [5|3]]),
   (7, [[4|4], [5|4], [5|5]]),
   (30, [[4|5], [4|6]]),
   (6, [[5|1], [5|2]]),
   (9, [[5|6], [6|6]]),
   (8, [[6|1], [6|2], [6|3]]),
   (2, [6|4], [6|5])
  ]
).
