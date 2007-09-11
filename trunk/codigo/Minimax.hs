module Minimax(
--	arbolDeMovidas, podar, minimax

        --, alfaBeta -- opcional
) where

data Arbol a = Nodo a [Arbol a]

foldArbol :: (a->[b]->b)-> Arbol a -> b
foldArbol g (Nodo n xs)  =	g n (map (foldArbol g) xs) 

foldArbol2 :: (a-> [b] ->b) -> ([b] -> [b]) -> Arbol a -> b
foldArbol2 g h (Nodo n ts) = g n (map (foldArbol2 g h) ts)

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f ar = foldArbol g ar 
	where g n xs = Nodo (f n) xs

foldNat :: b-> (b->b) -> Int -> b
foldNat fCero fN 0 = fCero 
foldNat fCero fN n = fN (foldNat fCero fN (n-1))

aPrueba = (Nodo 5 [(Nodo 4 [(Nodo 3 [Nodo 2 [(Nodo 1 [])]])])])

altura::Arbol a->Int
altura ar = foldArbol f ar 
	where f n r = 1 + (if (length r) > 0 then (maximum r) else 0)
	
--podar :: Int -> Arbol a -> Arbol a
--podar altura (Nodo n xs) = foldNat (Nodo n []) f altura
--	where f n arbol = podar (n-1) arbol

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
