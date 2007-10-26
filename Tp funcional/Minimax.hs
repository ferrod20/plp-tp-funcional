module Minimax(
	arbolDeMovidas, podar, minimax
        --, alfaBeta -- opcional
) where

import List

data Arbol a = Nodo a [Arbol a] deriving Show

----------------------------------------------------Funciones útiles
raiz (Nodo n xs) = n
mostrar (Nodo n xs) = "Nodo " ++ (show n) ++ " [" ++ (concat (map mostrar xs)) ++ "] "    
----------------------------------------------------Funciones útiles
foldArbol :: (a->[b]->b)-> Arbol a -> b
foldArbol g (Nodo n xs)  =	g n (map (foldArbol g) xs) 

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f ar = foldArbol g ar 
	where g n xs = Nodo (f n) xs

foldNat :: b-> (b->b) -> Int -> b
foldNat fCero fN 0 = fCero 
foldNat fCero fN n = fN (foldNat fCero fN (n-1))

podar :: Int -> Arbol a -> Arbol a
podar = foldNat (\(Nodo n _) -> Nodo n []) fN
	where fN g (Nodo n xs) = Nodo n (map g xs)		

----------------------------------------------------------------------------------------------------------
--arbolDeMovidas
aArbol::[a]->[Arbol a]
aArbol xs = map (\x->Nodo x []) xs

arbolDeMovidas :: (a -> [a]) -> a -> Arbol a
arbolDeMovidas f x = aM f (Nodo x [])

aM::(a -> [a])->Arbol a->Arbol a
aM f (Nodo n xs) = Nodo n  (map (aM f) (aArbol(f n)) )
----------------------------------------------------------------------------------------------------------
--A partir del arbol minimax, elige el nodo correcto del arbol original
minimax :: Ord b => (a -> b) -> (a -> Bool) -> Arbol a -> a
minimax feval turnoMax (Nodo a xs) = g (arbolMinimax feval turnoMax (Nodo a xs) )
	where g (Nodo n ys) = 
		if (null ys)
		then a
		else 
			if (turnoMax n) 
			then raiz( xs!!(posMaximo feval ys) )
			else raiz( xs!!(posMinimo feval ys) )

--Genera el arbol minimax
arbolMinimax :: Ord b => (a -> b) -> (a -> Bool) -> Arbol a -> Arbol a
arbolMinimax feval turnoMax (Nodo a xs) = foldArbol g (Nodo a xs)
	where g n ys = 
		if (null ys) 
		then Nodo n []
		else 
			if (turnoMax n) 
			then Nodo (raiz (maximo feval ys) ) ys
			else Nodo (raiz (minimo feval ys) ) ys
			
posMaximo f xs = [i |i<-[0..length xs -1], and [ f(raiz (xs!!i)) >= f(raiz (xs!!j))|j<-[0..length xs -1]]]!!0
posMinimo f xs = [i |i<-[0..length xs -1], and [ f(raiz (xs!!i)) <= f(raiz (xs!!j))|j<-[0..length xs -1]]]!!0
maximo f xs = xs!!(posMaximo f xs)
minimo f xs = xs!!(posMinimo f xs)
--------------------------------------------------------------------------------De aca para abajo, todo es prueba......

avacio = Nodo 0 [] --arbol de altura 1 (no hay nil...)
aPrueba = (Nodo 1 [(Nodo 4 [(Nodo 3 [Nodo 2 [(Nodo 1 [])]])])])       --arbol de altura 5
aPrueba2 = ( Nodo 2 [(Nodo 4 []) ,(Nodo 4 []) , (Nodo 4 []) , (Nodo 4 [])] ) --arbol de altura 2
aPrueba3 = (Nodo 3 [aPrueba2, (Nodo 4 [(Nodo 3 [Nodo 2 [(Nodo 1 [])]])])])       --arbol de altura 5

a9D = Nodo 9 [Nodo 6 [], Nodo 10 []]
a9I = Nodo 13 [Nodo 10 [],Nodo 4 [Nodo 7 [],Nodo 3 []],Nodo 14 [],Nodo 8 []]
aM10 = Nodo 10 [a9I, a9D]
aM2 = Nodo 2 [Nodo 3 [],Nodo 1 [],Nodo 11 []]
aM6 = Nodo 6 [Nodo 5 [Nodo 6 [Nodo 9 [],Nodo 5 []], Nodo 2 []]]
aMinimax = (Nodo 1 [aM6, aM10, aM2])

---------------------------------------Prueba foldArbol
altura::Arbol a->Int
altura ar = foldArbol f ar 
	where f n r = 1 + (if (length r) > 0 then (maximum r) else 0)

pA = altura a9D
pA2 = altura avacio
---------------------------------------Prueba mapArbol
pMap1 = mapArbol (\x->x*2) a9D 
pMap2 = mapArbol  (+2) a9I
---------------------------------------Prueba foldNat
contar = foldNat 0 (\x->x+1)
pFN1 = contar 4
pFN2 = contar 1
pFN3 = contar 0
---------------------------------------Prueba podar
pP1 = podar 2 aPrueba2
pP2 = podar 1 aPrueba2
pP3 = podar 3 aPrueba2
---------------------------------------Prueba arbolDeMovidas
pAM = podar 2 (arbolDeMovidas (\x -> [x - 1, x, x + 1]) 0)
pAM2 = podar 3 (arbolDeMovidas (\x -> [x - 1, x, x + 1]) 0)
---------------------------------------Prueba minimax
--Idea, armamos un arbol que en los niveles intercambia entre numeros pares e impares y la funcion de evaluacion maximiza si es impar.
pMin = minimo (id) [aM6, aM10, aM2]
pMax = maximo (id) [aM6, aM10, aM2]
posMin = posMinimo (id) [aM6, aM10, aM2]
posMax = posMaximo (id) [aM6, aM10, aM2]
pM1 = arbolMinimax (id) (\x->rem x 2== 1) aM2
pM2 = arbolMinimax (id) (\x->rem x 2== 1) aM6
pM3 = arbolMinimax (id) (\x->rem x 2== 1) aM10
pM4 = arbolMinimax (id) (\x->rem x 2== 1) aMinimax
pM5 = minimax (id) (\x->rem x 2== 1) aMinimax
-- aMinimax.....
--  		---------------------1-----------------------
--  		|					 |      	   			|
--     -----6			--------10--------		    ---	2---
--     |				|	  			  |         |   |   |
--     5---	     -------13------	  ----9			3	1 	11	
--     |  |      |	 |	    |  |      |   |	  	
-- 	---6  2      10  4---  14  8	  6  10             
-- 	|  |       	     |  |  		
-- 	9  5	     	 7  3
				
--
