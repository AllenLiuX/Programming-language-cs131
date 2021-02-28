%dotests :-
%    findall(T0, test0(T0), T0s), maplist(portray_clause, T0s), nl,
%    test1(T1), portray_clause(T1), nl,
%    test2(T2), portray_clause(T2), nl.
%
%:- initialization(dotests).

%plain_check_constraint(_, +(0, [])).
%plain_check_constraint(T, +(S, [Head|Tail])) :-
%    get_val(Head, T, N),
%    Temp is (N + Sum),
%    S =:= Temp,
%    plain_check_constraint(T, +(Sum, Tail)).    % recursion

plain_check_constraint(T, +(Res, L)) :-
        plainAdd(T, Res, L, 0).
plainAdd(_, Res, [], Res).
plainAdd(T, Res, [Head|Tail], Itr) :-
    get_val(Head, T, V),
    Itr2 is Itr + V,
    plainAdd(T, Res, Tail, Itr2).

%plain_check_constraint(_, *(1, [])).
%plain_check_constraint(T, *(P, [Head|Tail])) :-
%    get_val(Head, T, N),
%    P =:= (N * Prod),
%    plain_check_constraint(T, *(Prod, Tail)).     % recursion

plain_check_constraint(T, *(Res, L)) :-
        plainMult(T, Res, L, 1).
plainMult(_, Res, [], Res).
plainMult(T, Res, [Head|Tail], Itr) :-
    get_val(Head, T, V),
    Itr2 is Itr * V,
    plainMult(T, Res, Tail, Itr2).

%plain_check_constraint(T, -(D, J, K)) :-
%    get_val(J, T, M),
%    get_val(K, T, N),
%    (N =:= (D+M); M =:= (D+N)).       % Or
%
%plain_check_constraint(T, /(Q, J, K)) :-
%    get_val(J, T, M),
%    get_val(K, T, N),
%%    (N =:= Q * M; M =:= Q * N).       % Or
%    (N =:= (Q*M); M =:= (Q*N)).       % Or

plain_check_constraint(T, -(Res, J, K)) :-
    plainSub(T, Res, J, K).

plainSub(_, Res, _, _, Res).
plainSub(T, Res, J, K) :-
    get_val(J, T, A),
    get_val(K, T, B),
    (Itr is  A - B; Itr is B - A),
    plainSub(T, Res, J, K, Itr).

plain_check_constraint(T, /(Res, J, K)) :-
    plainDiv(T, Res, J, K).

plainDiv(_, Res, _, _, Res).
plainDiv(T, Res, J, K) :-
    get_val(J, T, A),
    get_val(K, T, B),
    (Itr is  A / B; Itr is B / A),
    plainDiv(T, Res, J, K, Itr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Requires a second definition because subtraction is not commutative
%plainSub(T, Res, J, K) :-
%    get_val(J, T, A),
%    get_val(K, T, B),
%    New_accumulator is  B - A,
%    plainSub(T, Res, J, K, Itr).

% Requires a second definition because division is not commutative
%plainDiv(T, Res, J, K) :-
%    get_val(J, T, A),
%    get_val(K, T, B),
%    Itr is B / A,
%    plainDiv(T, Res, J, K, Itr).