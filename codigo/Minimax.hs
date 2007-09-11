module Minimax(
	arbolDeMovidas, podar, minimax

        --, alfaBeta -- opcional
) where

-- Minimax

data Arbol a = Nodo a [Arbol a]

foldArbol :: (a->c->b) -> (b->c->c) -> c -> AG a -> b
foldArbol g f z (Nodo a xs) =	g a (foldr f z (map (foldArbol g f z) xs)))
-- u otra opción
-- foldArbol :: (a->c->b) -> ([b]->c) -> AG a -> b
-- foldArbol g h (GNode a ts) =	g a (h (map (foldAG2 g h) ts))
-- NO SE SI SE PUEDE DEFINIR FOLDARBOL CON MAP YA QUE EN EL PUNTO 12 PIDE DEFINIR MAPARBOL USANDO FOLDARBOL...


mapArbol :: (a -> b) -> Arbol a -> Arbol b
-- mapArbol f (Nodo a []) = f a
-- mapArbol f (Nodo a (x:xs)) = Nodo f a (mapArbol f [x] : mapArbol f xs)  MAL
-- PRIMERO HAY QUE DEFINIR FOLDARBOL SIN MAP??


iterateArbol :: (a -> [a]) -> a -> Arbol a

foldNat :: (b -> b) -> b -> Int -> b		Utiliar tipo Integer de Haskell ¿?
foldNat s z 0 = 0
foldNat s z n = s (foldNat s z (n-1))

podar :: Int -> Arbol a -> Arbol a

arbolDeMovidas :: (a -> [a]) -> a -> Arbol a

minimax :: Ord b =>
  (a -> b) ->     -- funcion de evaluacion
  (a -> Bool) ->  -- turno del jugador que maximiza?
  Arbol a -> a

------------------------------------------------------------------------------

-- Ejercicio opcional: Alpha-beta pruning

alfaBeta :: (Ord b, Num b) =>
  (a -> b) ->      -- funcion de evaluacion
  (a -> Bool) ->   -- turno del jugador que maximiza?
  b ->             -- alfa
  b ->             -- beta
  Arbol a -> a

