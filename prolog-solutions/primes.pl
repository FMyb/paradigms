range(L, L, []).
range(N, L, [N | T]) :- N < L, N1 is N + 1, range(N1, L, T).

concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

init(MAXN) :- \+ generate(2, MAXN).

comp_tab(1).
take_comp(I, _, MAXN) :- I > MAXN, !.
take_comp(I, STEP, MAXN) :-
	I =< MAXN,
	assert(comp_tab(I)),
	IS is I + STEP,
	take_comp(IS, STEP, MAXN).

next_prime(I, NEXT) :-
	not comp_tab(I),
	NEXT is I, !;
	I1 is I + 1,
	next_prime(I1, NEXT).

generate(I, MAXN) :-
	J is I * I,
	J =< MAXN,
	take_comp(J, I, MAXN),
	I1 is I + 1,
	next_prime(I1, NEXT),
	generate(NEXT, MAXN).

prime(N) :- \+ comp_tab(N), !.
composite(N) :- N > 2, comp_tab(N), !.

check_mod(A, B) :-
	M is A mod B,
	M = 0.

calc([], 1).
calc([H | T], R) :-
	calc(T, R1),
	R is H * R1.

check([A], 0).
check([A, B], A) :- 
	integer(A),
	integer(B),
	\+ (A > B), !.
check([H | T], H) :-
	check(T, R1),
	\+ (H > R1), !.


min_prime_divisors(N, I, R) :- 
	I * I =< N,
	check_mod(N, I),
	prime(I),
	R is I;
	I1 is I + 1,
	min_prime_divisors(N, I1, R). 
	
prime_divisors(1, []) :- !.
prime_divisors(N, [N]) :- prime(N), !.
prime_divisors(N, [H | T]) :-
	integer(N),
	min_prime_divisors(N, 2, H),
	D is div(N, H),
	prime_divisors(D, T), !.

	
prime_divisors(N, Divisors) :-
	check(Divisors, R),
	calc(Divisors, N), !.

change(0, _, []) :- !.
change(N, K, R) :-
	N > 0,
	M is N mod K,
	D is div(N, K),
	change(D, K, R1),
	concat(R1, [M], R), !.

prime_palindrome(N, K) :-
	prime(N),
	change(N, K, R),
	reverse(R, R1),
	R = R1.

