module Minimax(
--	arbolDeMovidas, podar, minimax

        --, alfaBeta -- opcional
) where

data Arbol a = Nodo a [Arbol a]

foldArbol :: (a->[b]->b)-> Arbol a -> b
foldArbol g (Nodo n xs)  =	g n (map (foldArbol g) xs) 

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f ar = foldArbol g ar 
	where g n xs = Nodo (f n) xs

foldNat :: b-> (b->b) -> Int -> b
foldNat fCero fN 0 = fCero 
foldNat fCero fN n = fN (foldNat fCero fN (n-1))

avacio = Nodo 0 [] --arbol de altura 1 (no hay nil...)
aPrueba = (Nodo 5 [(Nodo 4 [(Nodo 3 [Nodo 2 [(Nodo 1 [])]])])])       --arbol de altura 5
aPrueba2 = ( Nodo 5 [(Nodo 4 []) ,(Nodo 4 []) , (Nodo 4 []) , (Nodo 4 [])] ) --arbol de altura 2

altura::Arbol a->Int
altura ar = foldArbol f ar 
	where f n r = 1 + (if (length r) > 0 then (maximum r) else 0)
	
	
--usar un filter + altura ..por ahora no funka...
--podar :: Int -> Arbol a -> Arbol a
--podar n ab = foldArbol (\n xs -> filter (\x -> altura x <= n) xs ) ab

-- FER-------------------------------------------------
--Ayer pude consultar sobre la funcion podar del TP.
--Lo que saque en claro es lo siguiente:
--1) Se hace con foldNat.
--2) Se hace de la siguiente manera
	
--podar :: Int -> Arbol a -> Arbol a
--podar alt (Nodo n xs) = foldNat (Nodo n []) (agregarNivel (Nodo n xs)) alt 
--	where agregarNivel arbolOriginal arbolTamN-1
  
--3) Falta hacer agregarNivel (q me dijo que la haga yo....creo q era mucho la haga toda el....aunque pensandolo bien....)
 
--Idea: agregarNivel toma el arbol original y un arbol de tamaño n-1, yo tengo que crear el arbol de altura n....
--Como??? Utilizo el arbol original
--Entonces el problema se reduce a: hacer una funcion que dados 2 arboles ( el original y uno de altura n-1), devuelve el arbol de altura n. 
--Idea: recorrerlos e ir viendo cuando el original tiene hijos y el otro no.....Ahi ponerle al otro hijos de altura 1.
-- FER-------------------------------------------------	

--En este también consulté y vale recursion explicita, creo que asi esta bien.
aArbol::[a]->[Arbol a]
aArbol xs = map (\x->Nodo x []) xs

arbolDeMovidas :: (a -> [a]) -> a -> Arbol a
arbolDeMovidas f x = aM f (Nodo x [])

aM::(a -> [a])->Arbol a->Arbol a
aM f (Nodo n xs) = Nodo n  (map (aM f) (aArbol(f n)) )

--foldArbol (g altura) ar
--	where g altura n xs  = foldNat (Nodo n []) f2
--	f2 ent ar = (Nodo n xs) 

--arbolDeMovidas :: (a -> [a]) -> a -> Arbol a
--
--minimax :: Ord b =>
--  (a -> b) ->     -- funcion de evaluacion
--  (a -> Bool) ->  -- turno del jugador que maximiza?
--  Arbol a -> a
--
--------------------------------------------------------------------------------
--
---- Ejercicio opcional: Alpha-beta pruning
--
--alfaBeta :: (Ord b, Num b) =>
--  (a -> b) ->      -- funcion de evaluacion
--  (a -> Bool) ->   -- turno del jugador que maximiza?
--  b ->             -- alfa
--  b ->             -- beta
--  Arbol a -> a
--

 
