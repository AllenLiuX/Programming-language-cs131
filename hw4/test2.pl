%sortie([]).
%sortie([_]).		% X is replaced with _ since they all match pattern of one single element
%sortie([X|L]) :- allge(L,X).
%allge([],Y).
%% "allge([X], Y) :- X>=Y"  is removed because allge can only be called by previous 'allge(L,X)' with at least one element in L, and such case is dealt with by the next line
%allge([X|L], Y) :- X >= Y, allge(L,X).


%ismem(L) :- member(L, L).

sorted([]).
sorted([_]).
sorted([X,Y|Z]) :- X =< Y, sorted([Y|Z]).

mymember(X, X) :- X = X.
mymember(X, [X|_]).
mymember(X, [_|L]) :- mymember(X,L).
%findall(L, mymember(L,L), Res).


refal(X) :- X=2.

sortie([]).
%sortie([_]).
sortie([X|L]) :- allge(L,X).
allge([],Y).
% allge([X], Y) :- X>=Y
allge([X|L], Y) :- X >= Y, allge(L,X).


has_zero_one([hd|tl]) :-
    member(0, hd),
    !,
    member(1, hd).
has_zero_one([hd|tl]) :-
    has_zero_one(tl).

%has_zero_one([[0,2],[1,0]]).
%“contain_zero_and_one([[0,2],[1,0]]).” fails when it should succeed.

correct_time(time3).
correct_time(time1).
correct_time_2(time1).
relax(X):-correct_time(X),!,correct_time_2(X).