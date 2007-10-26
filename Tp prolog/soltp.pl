%Los celulares actuales tienen incorporado un dicionario de palabras y un algoritmo que va restringiendo las posibles palabras que puedan ser resultado de la presion de una serie de teclas del aparato.
%El objetivo del TP es representar el comportamiento predictivo de los mensajes de texto de los celulares.
%Para ello se definen dos predicados que brindan la informacion necesaria del telefono celular:
%1. Se define el mapeo entre las teclas de celular y los caracteres de la siguiente manera: 
teclado([ (1, [1]), (2, [2,a,b,c]), (3, [3,d,e,f]),
(4, [4,g,h,i]), (5, [5,j,k,l]), (6, [6,m,o,n]),
(7, [7,p,q,r,s]), (8, [8,t,u,v]), (9, [9,w,x,y,z]),
(0, [0]), (*, [-]), (#, [#])
]).
%es decir con una lista de tuplas (D, Xs) donde D es el digito presionado y Xs es la lista de caracteres que puede representar dicho digito. Los caracteres se representan por medio de un atomo o de un numero entero. Notar que el caracter " " esta representado por el atomo "-"
%2. El diccionario de palabras se representa de la siguiente manera:
diccionario([ 
[1,a], [l,a], [c,a,s,a], [a], [d,e], [r,e,j,a], [t,i,e,n,e],
[c,a,s,a,m,i,e,n,t,o], [d,e,l], [a,n,t,e,s]
]).
%es decir una lista de lista de caracteres que representan las palabras conocidas. Puede asumirse que ninguna palabra del diccionario contiene el atomo "-".
%1. Predicados pedidos
%
%Se pide definir los siguientes predicados, respetando la instanciacion pedida y de tal manera que no se devuelvan soluciones repetidas:
%
%1. teclasNecesarias(+Palabra, -ListaDigitos) donde Palabra es una lista de caracteres y tiene exito si la ListaDigitos es la lista con los digitos que deben presionarse para obtenerla. Ejemplo:
%
%?- teclasNecesarias([c,a,s,a], Ds).
%Ds = [2, 2, 7, 2] ;
%No
%
teclasNecesarias([],[]).
teclasNecesarias([X|Xs],Ys):- teclaNecesaria(X,T), teclasNecesarias(Xs,Ys), append([T] ,YYs, Ys).

teclaNecesaria(Caracter,Tecla):- teclado(T), obtTecla(Caracter, T, Tecla).

obtTecla( Caracter,[], _ ):-!. 
obtTecla( Caracter,[(Tecla,Cs)|Xs], Tecla ):- member(Caracter, Cs).
obtTecla( Caracter,[(T,Cs)|Xs], Tecla ):- obtTecla(Caracter,Xs,Tecla).



%2. palabraPosible(+ListaDigitos, -Palabra) donde ListaDigitos es una lista de teclas presionadas y tiene exito si Palabra es una palabra del diccionario, y con las teclas presionadas se obtiene esa palabra o un prefijo de la misma.
%Ejemplo:
%
%?- palabraPosible([2], P).
%P = [a] ;
%P = [a, n, t, e, s] ;
%P = [c, a, s, a] ;
%P = [c, a, s, a, m, i, e, n, t, o] ;
%No
%
%3. todasLasPalabrasPosibles(+ListaDigitos, -Palabras) donde ListaDigitos es una lista de teclas presionadas y tiene exito si Palabras es una lista de palabras del diccionario tal que las teclas presionadas generan una lista de caracteres que puede ser prefijo de la mismas.
%Nota: tener en cuenta que la solucion debe ser vista como un conjunto. O sea, soluciones con las mismas palabras pero en distinto orden deben ser consideradas como iguales (no deben devolverse repetidos). Ejemplo:
%
%?- todasLasPalabrasPosibles([2], Ps).
%Ps = [[a], [a, n, t, e, s], [c, a, s, a], [c, a, s, a, m, i, e, n, t, o]] ;
%No
%
%?- todasLasPalabrasPosibles([2], [[c, a, s, a],[a], [a, n, t, e, s],
%[c, a, s, a, m, i, e, n, t, o]]).
%Yes
%
%4. oracionPosible(+ListaDigitos, -Oracion) donde ListaDigitos es una lista de teclas presionadas que puede incluir el * (que se mapea al espacio), y tiene exito si se formo una oracion correcta. Una oracion es correcta si cada una de las secuencias de teclas entre los * puede formar una palabra del diccionario (es decir una palabra como en los items anteriores). Ejemplo:
%
%?- oracionPosible([2,*,3], O).
%O = [a, -, d, e] ;
%O = [a, -, d, e, l] ;
%O = [a, n, t, e, s, -, d, e] ;
%O = [a, n, t, e, s, -, d, e, l] ;
%O = [c, a, s, a, -, d, e] ;
%O = [c, a, s, a, -, d, e, l] ;
%O = [c, a, s, a, m, i, e, n, t, o, -, d, e] ;
%O = [c, a, s, a, m, i, e, n, t, o, -, d, e, l] ;
%No
%
%%----------------------------descartado-----------------------------------
%1.1. Juego adicional (opcional) 
%%----------------------------descartado-----------------------------------





%not(+Goal)

%append(xs, ys, zs) -> zs = xs++ys (Dice si zs unifica con xs++ys)
%member(e, xs) -> e pertenece a xs (dice si e unifica con algun elemnto de xs)
%nth0(i, xs, e): Dice si xs[i] unifica con e.
%last(xs, e): Dice si el ult elemento de xs unifica con e.
%sumlist(xs, sum): Hace la sumatoria de xs y unifica con sum.
%delete(xs, e, ys) elimina todos los miembros de xs que unifican con e y con ys.
%select(e, xs, r) Selecciona e de xs dejando el resto r...???
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



%sublist(pred, List1, List2): filtra List1 con pred, unifica en List2    
%maplist(+Pred, +List): Se aplica pred a cada elemento de la lista hasta el final o hasta que pred de falso.    
%maplist(+Pred, ?List1, ?List2): Se aplica pred a cada par de elementos de List1 y List2 hasta que pred de falso.        
%maplist(+Pred, ?List1, ?List2, ?List3): Idem anterior para triplas...    

%setof(+Template, +Goal, -Set).......falta!!
    


%Meta-predicados: bagof, setof, maplist, sublist, not, var, nonvar