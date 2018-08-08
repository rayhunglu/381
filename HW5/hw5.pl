% CS381, HW5
% Name: 1. Han-Yu Wu
%       2. Po-Ying Chao
%       3. Jui-Hung Lu
%       4. Chi Wen
%       5. Chih-Hsiang Wang

% Exercise 1. Database Application

when(275, 10).
when(261, 12).
when(381, 11).
when(398, 12).
when(399, 12).

where(275, owen102).
where(261, dear118).
where(381, cov216).
where(398, dear118).
where(399, cov216).

enroll(mary, 275).
enroll(john, 275).
enroll(mary, 261).
enroll(john, 381).
enroll(jim, 399).

% (a)
schedule(N,P,T) :- enroll(N,C), where(C,P), when(C,T).

% (b)
usage(P,T) :- where(C,P), when(C,T).

% (c)
conflict(X,Y) :- when(X,T), when(Y,T), where(X,P), where(Y,P), X\=Y.

% (d)
meet(X,Y) :- enroll(X,C), enroll(Y,C), X\=Y.
meet(X,Y) :- schedule(X,P,T1), schedule(Y,P,T2), T1 =:= T2+1, X\=Y.
meet(X,Y) :- schedule(X,P,T1), schedule(Y,P,T2), T1 =:= T2-1, X\=Y.


% Exercise 2. List Predicates and Arithmetic

% (a)
rdup([],[]).
rdup([X],[X]).
rdup([X,X|Ls],M) :- rdup([X|Ls],M).
rdup([X,Y|Ls],[X|Ms]) :- rdup([Y|Ls],Ms), X\=Y.

% (b)
flat([],[]).
flat([X|Ls],M):- append(X,L,N),flat(N,M).
flat([X|Ls],[X|Ms]):- \+ is_list(X), flat(L,Ms).

% (c)
project(_,[],_).
project([],_,_).
project(0,[H|_],[H|_]).
project1(1,[X|_],X) :- !.
project1(N,[_|L],X) :- \+ is_list(N), N1 is N-1, project1(N1,L,X).
project1([N1|N],L,[X1|XR]) :- project1(N1,L,X1), project1(N,L,XR).
project(A,B,M) :- project1(A,B,L), flat(L,M).
