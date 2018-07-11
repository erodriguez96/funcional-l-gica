posPares([],[]) :- !.

posPares([_],[]) :- !.

posPares([_,H2|T], [H2|T2]) :- posPares(T,T2).

% posImpares([],[]) :- !.

% posImpares([X],[X]) :- !.

% posImpares([H,H2|T], [H|T2]) :- posPares(T,T2).

%not working
aplana([],[]).

aplana(X,[X]).

aplana([H|T], R) :-
    aplana(H,T2),
    aplana(T,T3),
    append(T2,T3,R).
    
    
contenida([], []):-!.
contenida([], [_|_]):-!.

contenida( [H|T], [H2|T2]) :-
    (auxiliar([H|T], [H2|T2]) -> ! ; contenida([H|T], T2)).
    
auxiliar([], _):- 
    true, 
    !.

auxiliar([H|T], [H2|T2]):-
    (([H] == [H2]) -> auxiliar(T,T2) ; false).

trenza([],X,X) :- !.
trenza(X,[],X) :- !.
trenza([H|T],[H2|T2],[H,H2|R]) :-
    trenza(T,T2,R).
    

firstn([],_,[]) :- !.

firstn(_, Numero, []):-
    Numero =< 0,
    !.

firstn([H|T], Numero, [H|Resultado]):-
    Numero2 is Numero - 1,
    firstn(T,Numero2,Resultado).


diferencia([],_,[]) :-!.
diferencia(_,[],_) :-!.

diferencia([H|T],[H2|T2],L) :-
    isContained(H,[H2|T2]), 
    diferencia(T,[H2|T2],L).
    
diferencia([H|T],[H2|T2],[H|L]) :-
    diferencia(T,[H2|T2],L).
    
isContained([],_):-true, !.
isContained(_,[]):-false, !.
isContained(X,[H|T]) :-
    (X == H -> true ; isContained(X,T)).


my_last([X],X):-!.
my_last([_|T],R):- 
    my_last(T,R).
    
my_last_but_one([_],[]):-!.
my_last_but_one([H,_],[H]):-!.
my_last_but_one([_|T],R):-
    my_last_but_one(T,R).
    
element_at([],_,[]):-!.
element_at([H|_],1,[H]):-!.
element_at([_|T],X,R):-
    N is X - 1,
    element_at(T,N,R).
    
nelements([],0).
nelements([_|T],R):-
    nelements(T,N),
    R is N + 1.
    
my_reverse([],[]):-!.
my_reverse([X],[X]):-!.
my_reverse([H|T],R):-
    my_reverse(T,X),
    append(X,[H],R).



my_flatten([],[]) :- !.

my_flatten(X,[X]) :- 
    not(is_list(X)).
    
my_flatten([H|T],R):-
    my_flatten(H,R2),
    my_flatten(T,R3),
    append( R2 , R3 , R ).

trenza2([],X,X):-!.
trenza2(X,[],X):-!.
trenza2([H|T],[H2|T2],[H,H2|R]):-
    trenza2(T,T2,R).
    
invierte([],[]):-!.
invierte([X],[X]):-!.
invierte([H|T],R):-
    invierte(T,R2),
    append(R2,[H],R).
    
palindrome(L):-invierte(L,L).































