%Casos de prueba

teclas1([2]).
teclas2([2,*,3]).
palabra1([1,c]).

test0(Ds) :- teclasNecesarias([c,a,s,a], Ds).
test1(X) :- teclas1(Ts), palabraPosible(Ts, X).
test2a(X) :- teclas1(Ts), todasLasPalabrasPosibles(Ts, X).
test2b :- teclas1(Ts), todasLasPalabrasPosibles(Ts,  [[c, a, s, a],[a], [a, n, t, e, s]]).
test3(X) :- palabra1(P), teclasNecesarias(P, X).
test4a(X) :- teclas1(Ts), oracionPosible(Ts, X).
test4b(X) :- teclas2(Ts), oracionPosible(Ts, X).
