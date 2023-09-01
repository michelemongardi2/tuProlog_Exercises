%Realizzare un predicato "all" a due argomenti che mette in relazione un termine e una lista dove il termine identifica un template per tutti gli elementi della lista.
%Esempio: � vero che la lista � fatta da tutti "1"? all(1, [1,1,1]). 
%Ancora: all(p(_), [p(1), p(2), p(3)]). 
all(_, []).
all(H, [H2|T]) :- copy_term(H, H2), all(H, T).

%"Search" sugli alberi binari.
search(tree(_,E,_), E).
search(tree(L,_,_), E) :- search(L, E).
search(tree(_,_,R), E) :- search(R, E).

% ------------- LABORATORIO 11 ---------------
% ESERCIZIO 1.1
% Reimplement some of the previous exercises, but use built-in predicates (member, reverse, append, [H|T], [], ...) for lists and numbers
% search2 (Elem , List )
% looks for two consecutive occurrences of Elem
searchh(E, [E|T]) :- !.
searchh(E, [_|T]) :- searchh(E, T).

%Torna due elementi consecutivi
searchh2(E, [E,E|T]).
searchh2(E, [_|T]) :- searchh2(E, T).

% ESERCIZIO 1.2
% search_two (Elem , List )
% looks for two occurrences of Elem with any element in between
% Esempio: searchh_two(a,[b,c,a,a,d,e]). ? no
% searchh_two(a,[b,c,a,d,a,d,e]). ? yes
searchh_two(E, [E,_,E|T]).
searchh_two(E, [_|T]) :- searchh_two(E, T).

% ESERCIZIO 1.3
% size (List , Size )
% Size will contain the number of elements in List
%tail version
size(L, X) :- size(L, 0, X).
size([], Acc, Acc) :- !.
size([H|T], Acc, X) :- Acc2 is Acc+1, size(T, Acc2, X). 

%non tail
size_2([], 0).
size_2([_|T], N) :- size_2(T, N2), N is N2 + 1.

%Esercizio 1.4
% sum(List , Sum )
% Esempio: ?- sum ([1 ,2 ,3] ,X). 
%yes .
%X/6
sum(L,S) :- sum(L, 0, S).
sum([], Acc, Acc).
sum([H|T], Acc, S) :- Acc2 is Acc+H, sum(T, Acc2, S).

% sopra tail, sotto non-tail
sum2([], 0).
sum2([H|T], N) :- sum2(T, N2), N is N2 + H.

%Esercizio 1.5
% max(List ,Max , Min )
% Max is the biggest element in List
% Min is the smallest element in List
% Suppose the list has at least one element
min_max([H|T], X, Y) :- min_max([H|T], H, H, X, Y).
min_max([], MinTemp, MaxTemp, MinTemp, MaxTemp).
min_max([H|T], MinTemp, MaxTemp, X, Y) :- H>=MaxTemp, !, min_max(T, MinTemp, H, X, Y).
min_max([H|T], MinTemp, MaxTemp, X, Y) :- H=<MinTemp, !, min_max(T, H, MaxTemp, X, Y).  
min_max([H|T], MinTemp, MaxTemp, X, Y) :- min_max(T, MinTemp, MaxTemp, X, Y).  

%Esercizio 1.6
% sublist (List1 , List2 )
% List1 should contain elements all also in List2
% example : sublist ([1 ,2] ,[5 ,3 ,2 ,1]).
% Per restare pi� idiomatici potevamo usare la "member" al posto della search
sublist([], L).
sublist([H|T], L) :- searchh(H, L), sublist(T, L).

%ESERCIZIO 2.1
% dropAny (? Elem ,? List ,? OutList )
dropAny (X, [X | T], T).
dropAny (X, [H | Xs], [H | L]) :- dropAny (X, Xs , L).

%Esercizio 2.2
%Implement the following variations, by using minimal interventions: (cut and/or reworking the implementation)
% - dropFirst: drops only the first occurrence (showing no alternative results)
% - dropLast: drops only the last occurrence (showing no alternative results)
% - dropAll: drop all occurrences, returning a single list as a result

dropFirst(X, [X | T], T) :- !.
dropFirst(X, [H | Xs], [H | L]) :- dropFirst(X, Xs , L).

dropLast(X, [H | Xs], [H | L]) :- dropLast(X, Xs, L), !.
dropLast(X, [X | T], T).

dropAll(X, [], []).
dropAll(X, [X | T], L) :- !, dropAll(X, T, L).
dropAll(X, [H | Xs], [H | L]) :- dropAll(X, Xs,L).

%ESERCIZIO 3.1
% fromList (+ List ,- Graph )
fromList ([_] ,[]).
fromList ([H1 ,H2|T],[e(H1 ,H2)|L]):- fromList ([H2|T],L).

%It creates a graph from a list
% fromList([1,2,3],[e(1,2),e(2,3)]).
% fromList([1,2],[e(1,2)]).
% fromList([1],[]).

%ESERCIZIO 3.2
% fromCircList (+ List ,- Graph )
%fromCircList([1,2,3],[e(1,2),e(2,3),e(3,1)]).
%fromCircList([1,2],[e(1,2),e(2,1)]).
%fromCircList([1],[e(1,1)]).

fromCircList([H|T], L2) :- fromCircList([H|T], H, L2).
fromCircList([H|[]], F, [e(H,F)]).
fromCircList([H1,H2|T], F, [e(H1,H2)|L]):- fromCircList([H2|T],F,L).

%ESERCIZIO 3.3
% outDegree (+ Graph , +Node , -Deg )
% Deg is the number of edges leading into Node
% Vuole sapere il numero di archi in uscita da un nodo
outDegree(L, N, X) :- outDegree(L, 0, N, X).
outDegree([], Acc, N, Acc).
outDegree([e(H1,H2)|T], Acc, N, X) :- H1=N, Acc2 is Acc+1, !, outDegree(T, Acc2, N, X).
outDegree([e(_,_)|T], Acc, N, X) :- outDegree(T, Acc, N, X).

%ESERCIZIO 3.4
% dropNode (+ Graph , +Node , -OutGraph )
% drop all edges starting and leaving from a Node
% use dropAll defined in 1.1?? 
%Si devono eliminare, dato un nodo di partenza, tutti gli archi che partono e arrivano su quel nodo.
dropAll2([], X, []).
dropAll2([X2 | T], X, L) :- copy_term(X, X2), !, dropAll2(T, X, L).
dropAll2([H | Xs], X, [H | L]) :- dropAll2(Xs, X, L).

dropNode(G,N,OG):- dropAll2(G,e(N,_),G2), dropAll2(G2,e(_,N),OG).

%ESERCIZIO 3.5
% reaching (+ Graph , +Node , -List )
% all the nodes that can be reached in 1 step from Node
% possibly use findall , looking for e(Node ,_) combined
% with member (? Elem ,? List )
reaching(G, N, L) :- findall(E, member(e(N,E), G), L).


%map che utilizza un termine di un goal recuperando il predicato dal programma: esempio, map([10,20,30], inc, L).
map([],_,[]).
map([H|T], P, [H2|T2]) :- G =.. [P,H,H2], once(G), map(T,P,T2).
inc(N,N2) :- N2 is N+1.

%esempio findAll: risolve N volte il goal che ha nel secondo argomento, inserisce i valori trovati ad ogni passaggio nel primo argomento e la lista delle soluzione nel'ultimo, in modo da restituire un solo risultato
findall(p(X,Y), append(X,Y,[10,20,30]),L).

%esempio della filter utilizzando una sintassi composta: passando un predicato direttamente come input, esempio: filter([10,20,30,40,50], X, X>25,LO)
filter(L,X,P,LO) :- findall(X, (member(X,L), once(P)), LO). 

%Esempio della fattoriale "cacheta" ovvero, salva il risultato con un "assert" nella teoria interna in modo da non ricalcolare il risultato se gi� � presente.
%chiamo con factorial(5,X). per fare il calcolo normale, withcache(factorial(5,X)). per cachere il risultato. 
factorial(0,1) :- !. 
factorial(X,Y) :- Xs is X-1, factorial(Xm,Y2), Y is Y2*X.

withcache(P) :- cache(P), !.
withcache(P) :- once(P), assert(cache(P)).




