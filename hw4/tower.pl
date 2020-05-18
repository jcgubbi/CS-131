% To count how many are visible in a row/column
visible([], _, 0).
visible([H|T], X, N) :- 
	H #< X,
	visible(T, X, N). 
visible([H|T], X, N) :-
	H #> X,
	Nnew #= N-1,
	visible(T, H, Nnew). 
visible(A,N) :-
	visible(A, 0, N). 

% To create a valid row/column
unique_list(N, List) :-
	length(List, N),
        fd_domain(List,1,N),
        fd_all_different(List).

% helper functions for plain
domain(List, Low, High) :-
	maplist(between(Low, High), List). 

all_different(0, []).
all_different(0,_) :- !,fail.
all_different(N, [H|T]) :-
	H = N, 
	Nnew is N-1,
	all_different(Nnew,T). 
all_different(_,[]) :- !,fail.

%unique_list but for plain
validTrans(_,[]).
validTrans(Perm,[HC|TC]) :- 
 	\+ permutation(HC,Perm),!,fail.
validTrans(Perm,[HC|TC]) :-
 	validTrans(Perm,TC).
plain_unique(N, Trans, Perm, List) :-
	length(List, N),
	permutation(Perm, List),
	validTrans(Perm,Trans).

%visible but for plain
plain_visible([], _, N,N).
plain_visible(_, _, N,C) :-
	C > N, !, fail. 
plain_visible([H|T], X, N, C) :-
        H < X,
        plain_visible(T, X, N, C).
plain_visible([H|T], X, N, C) :-
        H > X,
  	Cnew is C+1,
        plain_visible(T, H, N, Cnew).
plain_visible(A,N) :-
        plain_visible(A, 0, N, 0).

%valid but plain
plain_valid2([], []).
plain_valid2([H|_], [C|_]) :-
	\+ plain_visible(H, C),!,fail.
plain_valid2([_|T], [_|T2]) :-
	plain_valid2(T, T2).
valid_count(List, N) :-
	length(List,N),
	maplist(between(1,N),List).
plain_valid(T, C, N) :-
	valid_count(C,N),
	plain_valid2(T,C).

%count number of cols
colLen([], _). 
colLen([H|T], N) :-
	length(H, N),
	colLen(T, N).
%valid T and heigh list combo
validT([], []).
validT([H|T], [C|T2]) :-
	visible(H, C),
	validT(T, T2). 

% TA Code for Transpose!
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
% End TA Code

tower(N, T, C) :- 
	% make sure T is NxN
	length(T, N),
	colLen(T, N),
	% break up counts into 4 arrays and make sure 4xN
	counts(U,D,L,R) = C,
	length(U, N),
	length(D, N),
	length(L, N),
	length(R, N),
	% transpose allows us to look at up/down
	transpose(T,TT),
	% ensure both rows and columns are valid
	maplist(unique_list(N), T),
	maplist(unique_list(N), TT),
	% reverse allows us to look at down/right
	maplist(reverse, T, RT),
	maplist(reverse, TT, RTT),
	% check that up/down/left/right sums are ok
	validT(TT, U),
	validT(RTT, D),
	validT(T, L),
	validT(RT, R),
	% fd_labeling for all the elements of T
	maplist(fd_labeling, T).

plain_tower(N, T, C) :-
	length(T, N),
	colLen(T, N), 
	counts(U,D,L,R) = C,
	length(U, N),
	length(D, N),
	length(L, N),
	length(R, N),
	transpose(T,TT),
        % ensure both rows and columns are valid
    all_different(N, Perm),
	maplist(plain_unique(N, TT, Perm), T),
	maplist(plain_unique(N, T, Perm), TT),
	% reverse allows us to look at down/right
    maplist(reverse, T, RT),
    maplist(reverse, TT, RTT),
    % check that up/down/left/right sums are ok
    plain_valid(TT, U, N),   %must rewrite visible
    plain_valid(RTT, D, N),
    plain_valid(T, L, N),
    plain_valid(RT, R, N).

ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C), 
	T1 \= T2.

speedup(Speed) :-
	statistics(cpu_time, [S1|_]),
	tower(5, T,
         counts([5,1,2,3,2],
                [1,3,2,3,2],
                [2,4,3,2,1],
                [2,1,2,3,3])),
	statistics(cpu_time, [E1|_]),
	statistics(cpu_time, [S2|_]), 
	plain_tower(5, TF,
         counts([5,1,2,3,2],
                [1,3,2,3,2],
                [2,4,3,2,1],
                [2,1,2,3,3])),
	statistics(cpu_time, [E2|_]),
	T1 is E1-S1+1, %plus one is just to ensure no division by zero!
	T2 is E2-S2,
	Speed is T2/T1. 
