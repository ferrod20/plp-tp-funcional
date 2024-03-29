teclado([ (1, [1]), (2, [2,a,b,c]), (3, [3,d,e,f]),
(4, [4,g,h,i]), (5, [5,j,k,l]), (6, [6,m,o,n]),
(7, [7,p,q,r,s]), (8, [8,t,u,v]), (9, [9,w,x,y,z]),
(0, [0]), (*, [-]), (#, [#])
]).
diccionario([ 
[1,a], [l,a], [c,a,s,a], [a], [d,e], [r,e,j,a], [t,i,e,n,e],
[c,a,s,a,m,i,e,n,t,o], [d,e,l], [a,n,t,e,s]
]).

%-------------------------------------------------------------------------------------------------------------
teclasNecesarias([],[]).
teclasNecesarias([X|Xs],Ys):- teclaNecesaria(X,T) , teclasNecesarias(Xs,YYs) , append([T] ,YYs, Ys).

teclaNecesaria(Caracter,Tecla):- teclado(T), obtTecla(Caracter, T, Tecla).

obtTecla( _,[], _ ). 
obtTecla( Caracter,[(Tecla,Cs)|_], Tecla ):- member(Caracter, Cs).
obtTecla( Caracter,[(_,Cs)|Xs], Tecla ):- not( member(Caracter, Cs)), obtTecla(Caracter,Xs,Tecla).
%-------------------------------------------------------------------------------------------------------------

%Version anterior que era menos eficiente que la actual
%palabraPosible( Xs, Pal):- diccionario(D), member(Pal, D), prefijo(Pref,Pal), length(Pref,L), L>0, teclasNecesarias(Pref,Xs).

caracterPosible(T,C,[(Tecla,Cs)|_]):- T == Tecla, member(C,Cs).
caracterPosible(T,C,[(Tecla,_)|Xs]):- T \== Tecla, caracterPosible(T,C,Xs).

palabraPosible( Xs, P):- palPosible(Xs,Pal), diccionario(D),
sonPrefijo(D,Pal,Ys), member(P,Ys).

palPosible([], []).
palPosible([X|Xs], [Y|Ys]):- teclado(T), caracterPosible(X,Y,T),
palPosible(Xs,Ys).

sonPrefijo(Xs,Pal,Ys):-sublist(prefijo(Pal),Xs,Ys).

prefijo([],_).
prefijo([X|L],[X|M]):- prefijo(L,M).


%-------------------------------------------------------------------------------------------------------------
%Casos de test
% todasLasPalabrasPosibles([2], [[c, a, s, a],[a], [a, n, t, e, s],[c, a, s, a, m, i, e, n, t, o]]).
% todasLasPalabrasPosibles([2], Ps).
% todasLasPalabrasPosibles([7], Ps).

todasLasPalabrasPosibles([],[]).
todasLasPalabrasPosibles(Digits, Pal) :- nonvar(Pal),setof(P, palabraPosible(Digits,P), P1),sort(Pal,P1).
todasLasPalabrasPosibles(Digits, Pal) :- var(Pal),setof(P, palabraPosible(Digits,P), Pal).

%-------------------------------------------------------------------------------------------------------------
%Idea: cortar Xs por palabras; cada vez que aparece un *. Buscar las palabras posibles para esas teclas. Pegar las palabras posibles.
%Casos de test: oracionPosible([2,*,3], O).
oracionPosible(Xs,Ys):-oracionPosible(Xs,[],Ys).

oracionPosible([],O,O).
oracionPosible(Xs,Ys,Zs):-tomarEspacio(Xs,R),append(Ys,[-],Z),oracionPosible(R,Z,Zs).
oracionPosible(Xs,Ys,Zs):-length(Xs,L), L>0, not(tomarEspacio(Xs,_)),tomarTeclas(Xs,Ts,R), palabraPosible(Ts,Pal), append(Ys,Pal,Z), oracionPosible(R,Z,Zs).

%tomarTeclas(Xs,Ts,Rs): Toma todos los elementos de Xs hasta encontrar un *, estos elementos los guarda en Ts. En Rs quedan los elementos de Xs sin los que se guardaron en Ts.
%Ejemplo:
%?- tomarTeclas([2,3,*,3],Ts,Rs).
%Ts = [2, 3],
%Rs = [*, 3] 

tomarTeclas([],[],[]).
tomarTeclas([X|Xs],[],[X|Xs]):- X == * .
tomarTeclas([X|Xs],[X|Ys],Zs):- X \== * , tomarTeclas(Xs,Ys,Zs).

%tomarEspacio(Xs,Ys): Es verdadero si el primer elemento de Xs es un * y si Ys es igual a Xs sin ese elemento.
tomarEspacio([X|Xs],Xs):- X == * .
%-------------------------------------------------------------------------------------------------------------
%1.1. Juego adicional (opcional) 
%-------------------------------------------------------------------------------------------------------------





%not(+Goal)

%append(xs, ys, zs) -> zs = xs++ys (Dice si zs unifica con xs++ys)
%member(e, xs) -> e pertenece a xs (dice si e unifica con algun elemnto de xs)
%nth0(i, xs, e): Dice si xs[i] unifica con e.
%last(xs, e): Dice si el ult elemento de xs unifica con e.
%sumlist(xs, sum): Hace la sumatoria de xs y unifica con sum.
%delete(xs, e, ys) elimina todos los miembros de xs que unifican con e y con ys.
%select(e, xs, r) Selecciona e de xs dejando el resto r...??
%reverse(xs, ys)    da vuelta xs y unifica con ys
%numlist(Low, High, ys)    unifica ys a una lista [Low, Low +1, ...High]
%sort(xs, ys): ys unifica a una lista ordenada a partir de xs, se eliminan duplicados.
%length(xs, n): unifica en n la longitud de xs
%number(t): da true si t es un numero (floating point o integer)
%atom(t): da True si t es un atom.
%subtract(+Set, +Delete, -Result): Result = Set - Delete
%intersection(+Set1, +Set2, -Set3) Set3 = Set1 int. Set2    
%union(+Set1, +Set2, -Set3) Set3 = Set1 U Set2
%list_to_set(xs, set): elimina los primeros duplicados de xs, unificando en el conjunto set.
%subset(+Subset, -Set): da true si todos los elementos de subset son elementos de set
%var(t): es True if t is una variable libre.


%-----------No se pueden usar en el parcial ni en las practicas-----------------------------
%sublist(pred, List1, List2): filtra List1 con pred, unifica en List2    
%maplist(+Pred, +List): Se aplica pred a cada elemento de la lista hasta el final o hasta que pred de falso.    
%maplist(+Pred, ?List1, ?List2): Se aplica pred a cada par de elementos de List1 y List2 hasta que pred de falso.        
%maplist(+Pred, ?List1, ?List2, ?List3): Idem anterior para triplas....... .....   

%setof(+Template, +Goal, -Set) evalua todos los elementos de +Template en Goal+ y devuelve todos aquellos que hacen al predicado +Goal true en la lista -Set
%Meta-predicados: bagof, setof, maplist, sublist, not, var, nonvar
%-----------No se pueden usar en el parcial ni en las practicas-----------------------------

%------------------------------PRUEBAS------------------------------------------------------

%Casos de prueba

teclas1([2]).
teclas2([2,*,3]).
palabra1([1,c]).


%------------------------------------------------------------------------------------------------------------------------------------------------------------
test0(Ds) :- teclasNecesarias([c,a,s,a], Ds).
%Resultado:

%Ds = [2, 2, 7, 2] ;

%No
%------------------------------------------------------------------------------------------------------------------------------------------------------------

test1(X) :- teclas1(Ts), palabraPosible(Ts, X).
%Resultado:

%X = [c, a, s, a] ;

%X = [a] ;

%X = [c, a, s, a, m, i, e, n, t|...] ;

%X = [a, n, t, e, s] ;

%No
%------------------------------------------------------------------------------------------------------------------------------------------------------------
test2a(X) :- teclas1(Ts), todasLasPalabrasPosibles(Ts, X).
%Resultado:


%X = [[a], [a, n, t, e, s], [c, a, s, a], [c, a, s, a, m|...]] ;

%No
%------------------------------------------------------------------------------------------------------------------------------------------------------------
test2b :- teclas1(Ts), todasLasPalabrasPosibles(Ts,  [[c, a, s, a],[a], [a, n, t, e, s]]).
%Resultado:

%No
%le falta la palabra casamiento..por lo que deberia devolver NO, por lo que es correcto
%------------------------------------------------------------------------------------------------------------------------------------------------------------
%Test hecho por nosotros, ahora deberia devolver YES
test2c :- teclas1(Ts), todasLasPalabrasPosibles(Ts,  [[c, a, s, a],[a], [a, n, t, e, s],[c,a,s,a,m,i,e,n,t,o]]).
%Resultado:

%Yes
%------------------------------------------------------------------------------------------------------------------------------------------------------------
test2d :- teclas1(Ts), todasLasPalabrasPosibles(Ts,  [[c,a,s,a,m,i,e,n,t,o],[c, a, s, a],[a], [a, n, t, e, s]]).
%Resultado:

%Yes
%------------------------------------------------------------------------------------------------------------------------------------------------------------
test3(X) :- palabra1(P), teclasNecesarias(P, X).
%Resultado:

%X = [1, 2] ;

%No
%------------------------------------------------------------------------------------------------------------------------------------------------------------
test4a(X) :- teclas1(Ts), oracionPosible(Ts, X).
%Resultado:

%X = [c, a, s, a] ;

%X = [a] ;

%X = [c, a, s, a, m, i, e, n, t|...] ;

%X = [a, n, t, e, s] ;

%No
%------------------------------------------------------------------------------------------------------------------------------------------------------------

test4b(X) :- teclas2(Ts), oracionPosible(Ts, X).
%Resultado:

%X = [c, a, s, a, -, d, e] ;

%X = [c, a, s, a, -, d, e, l] ;

%X = [a, -, d, e] ;

%X = [a, -, d, e, l] ;

%X = [c, a, s, a, m, i, e, n, t|...] ;

%X = [c, a, s, a, m, i, e, n, t|...] ;

%X = [a, n, t, e, s, -, d, e] ;

%X = [a, n, t, e, s, -, d, e, l] ;

%No
%------------------------------FIN PRUEBAS------------------------------------------------------