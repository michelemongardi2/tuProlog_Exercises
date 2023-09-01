%Successivo: preso un X torno il successivo s(X); da vedere come un Sum Type con "zero" e "s(n: Nat)" come case class. 
succ(X, s(X)).
 
%Somma di due elementi 
sum(X, zero, X).
sum(X, s(Y), s(C)) :- sum(X, Y, C). 

%Predicato "maggiore"
greater(s(_), zero).	
greater(s(X), s(Y)) :- greater(X, Y). 

%precedente e successivo: prevnext(5) -> 4,6
prevnext(s(X), X, s(s(X))).

%intervallo di numeri compresi tra il primo numero e il secondo dato in input
intervall(N, _, N).
intervall(N1, N2, E) :- greater(N2,N1), intervall(s(N1), N2, E). 

% ---- LISTE ----
%restituisce SI/NO se l'elemento se presenente.
element_(E, cons(E, _)).
element_(E, cons(_, L)) :- element_(E, L).

%Sintassi ad-hoc per le liste.
find([H|_], H).
find([_|T], E) :- find(T, E).

%Posizione dell'elemento H
position([H|_], zero, H).
position([_|T], s(N), E) :- position(T, N, E).

%append di due liste
join([], L, L).
join([H|T], L, [H|O]) :- join(T, L, O).

%somma sulle liste
listSum([], 0).
listSum([H|T], N2) :- listSum(T, N), N2 is H+N.

% ---- ESERCIZI LAB10----
% -- ESERCIZIO1

%search su una lista
search(X, cons(X, _)).
search(X, cons(_, Xs)) :- search(X, Xs).
%looks for two consecutive occurrences of Elem
search2 (X, cons (X, cons (X, _))).
search2 (X, cons (_, Xs)) :- search2 (X, Xs).
% looks for two occurrences of Elem with any element in between
search3(X, cons(X, cons(_, cons(X, _)))).
search3(X, cons(_, Xs)) :- search3(X, Xs).
% looks for any Elem that occurs two times , anywhere
search3_2(X, cons(X, Xs)) :- search(X,Xs).
search3_2(X, cons(_, Xs)) :- search3_2(X,Xs).

% -- ESERCIZIO2
%size (List , Size )
%Size will contain the number of elements in List ,written using notation zero , s( zero ), s(s( zero ))..
size(nil, zero).
size(cons(_, T), s(N)) :- size(T, N).
% sum_list (List , Sum)
sum2(X, zero , X).
sum2(X, s(Y), s(Z)) :- sum(X, Y, Z).
sum_list(nil, zero).
sum_list(cons(H, T), S) :- sum_list(T, R), sum2(H, R, S).
% count (List , Element , NOccurrences )
% it uses count (List , Element , NOccurrencesSoFar , NOccurrences )
count(List, E, N) :- count(List , E, zero, N).
count(nil, E, N, N).
count(cons(E, L), E, N, M) :- count(L, E, s(N), M).
count(cons(H, L), E, N, M) :- count(L, E, N, M).

count_nonTail([],E,0).
count_nonTail([E|T],E,N2) :- !, count_nonTail(T,E,N), N2 is N+1.
count_nonTail([H|T], E, N) :- count_nonTail(T, E, N).
%Max: max(List , Max )-->realise it  by properly changing count
%Max is the biggest element in List
%Suppose the list has at least one element
max(cons(H,T), M) :- max(T, H, M).
max(nil, M, M).
max(cons(H,T), TM, M) :- H>TM, max(T, H, M).
max(cons(H,T), TM, M) :- H=<TM, max(T, TM, M).
%min-max
minmax(cons(H,T), Min, Max) :- minmax(T, H, H, Min, Max).
minmax(nil, TempMin, TempMax, TempMin, TempMax).
minmax(cons(H,T), TempMin, TempMax, Min, Max) :- H>=TempMax, minmax(T, TempMin, H, Min, Max).
minmax(cons(H,T), TempMin, TempMax, Min, Max) :- H=<TempMin, minmax(T, H, TempMax, Min, Max).

% -- ESERCIZIO3
% same (List1 , List2 )
% are the two lists exactly the same ?
same(L,L).
% all_bigger (List1 , List2 )
% all elements in List1 are bigger than those in List2, 1 by1
check_element(_, nil).
check_element(H, cons(X,Y)) :- greater(H,X), check_element(H,Y).
all_bigger(nil,_).
all_bigger(cons(H,T), L) :- check_element(H,L), all_bigger(T,L).
%sublist (List1 , List2 )
%List1 should contain elements all also in List2
%search su una lista
sublist(nil, _).
sublist(cons(H,T), L) :- search(H, L), sublist(T,L).

% --- ESERCIZIO4
% seq(N,E, List ) --> List is [E,E ,... ,E] with size N
% example : seq (s(s(s( zero ))), a, cons (a, cons (a, cons (a, nil )))
seq(zero, E, nil).
seq(s(N), E, cons(E,T)) :- seq(N, E, T).
% seqR (N, List )
seqR(zero, nil).
seqR(s(N), cons(N, T)) :- seqR(N, T).
% seqR2 (N, List ) --> is [0 ,1 ,... ,N -1]
seqR2(N, L) :- seqR2(N, zero, L).
seqR2(zero, Acc, nil).
seqR2(s(N), Acc, cons(Acc, T)) :- seqR2(N, s(Acc), T).

% ---ESERCIZIO 5
%last element of the list
last(nil, nil).
last(cons(H,nil), H).
last(cons(H,T), X) :- last(T, X).

%map +1
map(nil, nil).
map(cons(H,T), cons(s(H), X)) :- map(T, X).

%filter>0 
filter(nil, nil).
filter(cons(H,T), cons(H,X)) :- greater(H, zero), filter(T, X).
filter(cons(H,T), X) :- H = zero, filter(T, X).

%count >0
count(L, N) :- filter(L, X), count_(X,N).
count_(nil, zero).
count_(cons(H,T), s(N)) :- count_(T,N).

%find the first element !=0 in the list.
find(nil, nil).
find(cons(H,T), H) :- greater(H, zero).
find(cons(H,T), X) :- find(T, X).


%count gli elementi di una lista in input
countElementInList(nil, zero).
countElementInList(cons(H,T), s(N)) :- countElementInList(T,N).

%elementi da tenere rispetto alla dimensione della lista passata in input e al numero di elementi che si vogliono rimuovere
toKeep(L_Dim, zero, L_Dim).
toKeep(s(X), s(N), Y) :- toKeep(X, N, Y).

dropRight(L, N, L2) :- countElementInList(L, L_Dim), toKeep(L_Dim, N, Y), dropR(L, Y, L2).
dropR(nil, N, nil).
dropR(L, zero, nil). 
dropR(cons(H,T), s(Y), cons(H,O)) :- dropR(T, Y, O).

%altro modo per fare la dropRight
dropRight(L, 0, []).
dropRight([H|T], N, [H|L]) :- N2 is N-1, dropRight(T, N2, L).

%dropLeft: cancella i primi X elementi della lista
dropLeft(L, zero, L). 
dropLeft(cons(H,T), s(Y), L2) :- dropLeft(T, Y, L2).

%cancella finch� non trovi un valore uguale a "zero".
dropWhile(nil, nil).
dropWhile(cons(zero,T), T).
dropWhile(cons(H,T), L) :- greater(H,zero), dropWhile(T,L).

%partiton di una lista in due liste: parte maggiore di zero e parte uguale a zero
partition(nil, nil, nil).
partition(cons(H,T), cons(H,L1), L2) :- greater(H,zero), partition(T,L1,L2).
partition(cons(H,T), L1, cons(H,L2)) :- H = zero, partition(T,L1,L2).

%reversed: inverti una lista
reversed(L, LRvrs) :- reversed(L, nil, LRvrs).
reversed(nil, Acc, Acc).
reversed(cons(H,T), Acc, LRvrs) :- reversed(T, cons(H,Acc), LRvrs).

%take i primi X elementi
take(L, zero, nil).
take(cons(H,T), s(Y), cons(H,L)) :- take(T, Y, L).

%zip: unisci due liste, elemento con elemento sulla base dell'indice. Le liste devono avere la stessa lunghezza.
zip(nil, nil, nil).
zip(cons(H,T), cons(X,Y), cons(pair(H,X), L)) :- zip(T,Y,L).

% --- sum_list originale --
%sum_list(nil, zero).
%sum_list(cons(H, T), S) :- sum_list(T, R), sum(H, R, S). 
%diversa implementazione di sum_list
sommaLista(L, X) :- sommaLista(L, zero, X).
sommaLista(nil, Acc, Acc).
sommaLista(cons(H,T), Acc, X) :- sum(Acc, H, Y), sommaLista(T, Y, X).

%sequenzaReverse.
sequenzaR(zero, nil).
sequenzaR(s(N), cons(N,L)) :- sequenzaR(N, L).

%sequenza reverse a partire da 0.
seqq2(N, L) :- seqq2(N, zero, L).
seqq2(zero, Acc, nil).
seqq2(s(N), Acc, cons(Acc,L)) :- seqq2(N, s(Acc), L).

map3(nil, nil).
map3(cons(H,T), cons(s(H),L)) :- map3(T, L).

%moltiplicazione tra due numeri
mult(N, zero, zero).
mult(N, s(N2), M) :- mult(N, N2, S), sum(N, S, M).

%fattoriale
factorial(N, X) :- factorial(N, s(zero), X).
factorial(zero, Acc, Acc).
factorial(s(N), Acc, X) :- mult(Acc, s(N), Y), factorial(N,Y,X).

%fattoriale del prof:
fattoriale(zero, s(zero)).
fattoriale(s(N), O) :- fattoriale(N, O2), mult(O2, s(N), O).

% ----------------------------------------- LEZIONE 11 -------------------------------------------------------

%somma tail recursive
sum_t(L, X) :- sum_t(L, 0, X).
sum_t([], Acc, Acc).
sum_t([H|T], Acc, X) :- Y is Acc+H, sum_t(T,Y,X).

%member2 serve per fornire in output una lista in cui non � presente l'elemento passato in input.
member2([X|Xs], X, Xs).
member2([X|Xs], E, [X|Ys]) :- member2(Xs, E, Ys). 

%permutation: un po un casino. Da rivedere bene negli appunti.
permutation([],[]).
permutation(Xs, [X|Ys]) :- member2(Xs, X, Zs), permutation(Zs, Ys).

%Riprendiamo ora l'esempio dell'intervallo per capire come poterci muoversi su una griglia 9*9 (escludiamo l'ultimo elemento) se vogliamo spostarci in verticale e in orizzontale rispetto
%alla posizione in cui si � (sopra, sotto, destra, sinistra). 
intervalll(N, _, N).
intervalll(X, Y, Z) :- X1 is X+1, X1 < Y, intervalll(X1, Y, Z).

%neighbour: prese le coordinate di due celle mi dice se sono vicine orizzontalmente e verticalmente.
neighbour(A, B, A, B2) :- B2 is B+1.
neighbour(A, B, A, B2) :- B2 is B-1.
neighbour(A, B, A2, B) :- A2 is A+1.
neighbour(A, B, A2, B) :- A2 is A-1.

gridLink(N, M, link(X, Y, X2, Y2)) :-
	intervalll(0, N, X),
	intervalll(0, M, Y),
	neighbour(X, Y, X2, Y2), 
	X2 >=0, Y2 >= 0, X2 < N, Y2 < N.

%merge ordinato di due liste. Presenta un errore in quanto presenta matching spuria.
mergeError(Xs , [], Xs).
mergeError([] , Ys , Ys).
mergeError([X|Xs], [Y|Ys], [X|Zs ]) :- X < Y, mergeError (Xs , [Y | Ys], Zs).
mergeError([X|Xs], [Y|Ys], [Y|Zs ]) :- X >= Y, mergeError ([X | Xs], Ys , Zs).

%Questa � la soluzione della merge con il cut corretta.
merge(Xs, [], Xs) :- !.
merge([], Ys, Ys).
merge([X|Xs], [Y|Ys], [X|Zs]) :- X<Y, !, merge(Xs, [Y|Ys], Zs).
merge([X|Xs], [Y|Ys], [Y|Zs]) :- merge([X|Xs], Y, Zs).

%soluzione Max con il cut.
max2(cons(H,T), M) :- max2(T, H, M).
max2(nil, M, M).
max2(cons(H,T), TM, M) :- H>TM, !, max2(T, H, M).
max2(cons(H,T), TM, M) :- max2(T, TM, M).

%indice dell'elemento E
index_of ([E|_], E, 0) :- !.
index_of ([_|T], E, N2) :- index_of(T, E, N), N2 is N + 1.
