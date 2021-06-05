map_build([], nil) :- !.
map_build([(K, V) | Suf], R) :- map_build(Suf, R1), map_put(R1, K, V, R), !.

% node(K, V, P, L, R). K - Key, V - Value, P - priority, L - left son, R - right son.

split(nil, _, nil, nil) :- !.
split(node(TK, TV, TP, TL, TR), K, node(TK, TV, TP, TL, T1), T2) :-
	TK < K, !,
	split(TR, K, T1, T2).
split(node(TK, TV, TP, TL, TR), K, T1, node(TK, TV, TP, T2, TR)) :-
	TK >= K, !, 
	split(TL, K, T1, T2).

merge(T1, nil, T1) :- !.
merge(nil, T2, T2) :- !.
merge(node(T1K, T1V, T1P, T1L, T1R), node(T2K, T2V, T2P, T2L, T2R), node(T1K, T1V, T1P, T1L, R2)) :-
	T1P > T2P, !,
	merge(T1R, node(T2K, T2V, T2P, T2L, T2R), R2).
merge(node(T1K, T1V, T1P, T1L, T1R), node(T2K, T2V, T2P, T2L, T2R), node(T2K, T2V, T2P, R2, T2R)) :-
	T1P =< T2P, !,
	merge(node(T1K, T1V, T1P, T1L, T1R), T2L, R2).

insert(T, K, V, R) :- 
	split(T, K, T1, T2),
	rand_int(1000000, NP),
	merge(T1, node(K, V, NP, nil, nil), R1),
	merge(R1, T2, R), !.

find(T, K, TV) :-
	split(T, K, T1, T2),
	K1 is K + 1,
	split(T2, K1, node(TK, TV, TP, TL, TR), T4),
	TK = K, !.

remove(T, K, R) :-
	split(T, K, T1, T2),
	K1 is K + 1,
	split(T2, K1, T3, T4),
	merge(T1, T4, R), !.

map_get(T, K, V) :-
	find(T, K, V), !.

map_put(T, K, V, R) :-
	find(T, K, _), !,
	remove(T, K, R1),
	insert(R1, K, V, R).

map_put(T, K, V, R) :-
	\+ find(T, K, _), !,
	insert(T, K, V, R).

map_remove(T, K, R) :-
	find(T, K, _),
	remove(T, K, R), !.
map_remove(T, K, T) :-
	\+ find(T, K, _), !.


min(node(TK, TV, TP, TL, TR), TK) :-
	TL = nil, !.
min(node(TK, TV, TP, TL, TR), R) :-
	\+ TL = nil, !,
	min(TL, R).

map_ceilingKey(T, K, R) :-
	split(T, K, T1, T2), min(T2, R), !.
	
