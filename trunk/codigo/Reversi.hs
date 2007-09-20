module Reversi
(
	Juego(..), Color(..), Dimension, Coordenada, Tablero(..),
	turno, movidasValidas, enRango, dimension, tablero,
	puedeJugarEn, quienGano, ultimaCoordJugada, terminoElJuego,
	diferenciaNegrasBlancas
) 

where

import Char

data Color = Negro | Blanco deriving Eq

type Dimension = (Int, Int)
type Coordenada = (Int, Int)

data Juego = Comenzar Dimension
           | Poner Juego Coordenada

data Tablero = Tablero Dimension (Coordenada -> Maybe Color)

tableroInicial :: Dimension -> Tablero
tableroInicial (mx, my) = Tablero (mx, my) pos
  where
    mx2 = mx `div` 2
    my2 = my `div` 2
    pos coord
      | coord `elem` [(mx2 - 1, my2 - 1), (mx2, my2)] = Just Negro
      | coord `elem` [(mx2, my2 - 1), (mx2 - 1, my2)] = Just Blanco
      | otherwise = Nothing

-------------------------------------------------------------------------------  Observadores de tablero
dim (Tablero dim pos) = dim
color (Tablero dim pos) c = pos c
fColor (Tablero dim pos) = pos

-----------------------------------------------------------------------------------------------------------------------
---------------------------------------------funciones del TP - Modulo reversi-----------------------------------------
-----------------------------------------------------------------------------------------------------------------------

enRango :: Coordenada -> Dimension -> Bool
enRango   (a,b) (c,d) = a<c && b<d && a>=0 && b>=0

foldJuego :: (Dimension->b)->(b->Coordenada->b)->Juego->b
foldJuego f1 f2 (Comenzar d) = f1 d
foldJuego f1 f2 (Poner j c ) = f2 (foldJuego f1 f2 j) c

dimension :: Juego -> Dimension
dimension = foldJuego id  f2
	where f2 r c = r

-------------------------------------------------------------------------------  coordsQueInvierte------------------ 
--Idea:
--1. Tomo los puntos mas cercanos del mismo color que estan alineados ortogonalemente o diagonalemente al punto que quiero insertar.
--O sea, obtengo los puntos entre los cuales quedarían encerradas las piezas del otro color.
--2. A partir de los puntos anteriores, obtengo los puntos que quedan encerrados, verificando que sean contiguos y del color oponente.
--1. Se hace con la funcion primerosAlineados
--2. Se hace con la funcion coordEntre.
coordsQueInvierte :: Color -> Tablero -> Coordenada -> [Coordenada]
coordsQueInvierte col t c = coordEntre col t c  (primerosAlineados col t c ) 

--primerosAlineados k t c devuelve todos los puntos de color k mas cercanos a c que estan alineados ortogonalemente o diagonalmente al punto c en el tablero t.
--primerosAlineados Negro tabPrueba (1,3) da [(1,4),(2,3),(0,3),(0,4),(0,2),(1,0),(4,0)], donde tabPrueba es el tablero del enunciado
--primerosAlineados Blanco tabPrueba (1,3) da [(1,2),(2,4),(2,2),(1,5),(4,3)], donde tabPrueba es el tablero del enunciado
primerosAlineados:: Color -> Tablero -> Coordenada -> [Coordenada]
primerosAlineados k (Tablero (n,m) posic) (f,c) =  filter (masCercano puntos (f,c)) puntos
	where	puntos = [ a | i<-[0..hasta], a<-[(f,c+i),(f,c-i),(f+i,c),(f-i,c),(f+i,c+i),(f+i,c-i),(f-i,c+i),(f-i,c-i)], enRango a (n,m) && enRango (f,c) (n,m) && (posic a) == Just k ]			
		hasta = (max (n-1) (m-1))

--masCercano xs t c p dice si c es el punto mas cercano de todos los puntos xs al punto p
masCercano::[Coordenada]->Coordenada->Coordenada->Bool
masCercano xs p c = foldr (f c p) True xs
	where f c p x r = (masCerca c x p ) && r

----masCercano c x p dice si c esta mas cerca que x del punto p si los 3 puntos estan alineados	
masCerca:: Coordenada->Coordenada-> Coordenada->Bool
masCerca 	(a,b) (e,f) (c,d)
			|a>c && b>d 	= not(e>c && f>d ) ||	 (e>=a && f>=b)
			|a>c && b==d  	= not(e>c && f==d) ||  	 (e>=a && f==b)
			|a>c && b<d 	= not(e>c && f<d ) ||	 (e>=a && f<=b)
			|a<c && b>d     = not(e<c && f>d ) ||    (e<=a && f>=b)
			|a<c && b==d	= not(e<c && f==d) ||	 (e<=a && f==b)
			|a<c && b<d     = not(e<c && f<d ) ||    (e<=a && f<=b)
			|a==c && b>d	= not(e==c && f>d) ||	 (e==a && f>=b)	
			|a==c && b<d    = not(e==c && f<d) ||    (e==a && f<=b)

--coordEntre c xs: devuelve los puntos entre c y cada uno de los de xs. Para q funcione correctamente, cada uno de los xs debe estar alineado ortogonalemente o diagonalmente al punto c
coordEntre::Color->Tablero->Coordenada->[Coordenada]->[Coordenada]
coordEntre col t c xs = foldr f  [] xs
	where f x r	 =	if( sonConsecutivas c x ( dameCoordEntre (negado col) t c x )) 
		then ( dameCoordEntre (negado col) t c x ) ++ r 
		else r

--dameCoordEntre d h devuelve todas las coordenadas lineales entre d y h de color col.
dameCoordEntre::Color->Tablero->Coordenada->Coordenada->[Coordenada]
dameCoordEntre col (Tablero dim pos) (df,dc) (hf,hc) = [(f,c)| f<-[(min df hf)..(max df hf)], c<-[(min dc hc)..(max dc hc)], ( hc == dc || hf == df || abs(hc-c)==abs(hf-f) ) && (f,c) /= (df, dc) && (f,c) /= (hf, hc) && pos(f,c)==Just col]

sonConsecutivas::Coordenada->Coordenada->[Coordenada]->Bool
sonConsecutivas (df,dc) (hf,hc) xs = and [elem (f,c) xs| f<-[(min df hf)..(max df hf)], c<-[(min dc hc)..(max dc hc)], ( hc == dc || hf == df || abs(hc-c)==abs(hf-f) ) && (f,c) /= (df, dc) && (f,c) /= (hf, hc)]

negado Blanco = Negro
negado Negro = Blanco

-------------------------------------------------------------------------------  puedeJugarEn------------------------------------------
--puedeJugarEn k t c 
--Da True si la coordenada c está en el rango del tablero t, si no hay nada en la coordenada c y si ubicar un color k en la coordenada c invierte al menos una coordenada del color opuesto.
puedeJugarEn :: Color -> Tablero -> Coordenada -> Bool
puedeJugarEn k (Tablero d pos) c = (enRango c d) && ((pos c) == Nothing) && length( coordsQueInvierte k t c ) > 0
	where t = (Tablero d pos)

	
------------------------------------------------------------------------------- terminoElJuego----------------------------------------
	
--Da True si no hay ninguna movida para realizar. Se fija en todas las posiciones si se puede poner una ficha blanca o una negra.
terminoElJuego :: Juego -> Bool
terminoElJuego j = not(hayMovidasPosibles Blanco (tablero j) ) && not(hayMovidasPosibles Negro (tablero j) )

		
hayMovidasPosibles :: Color->Tablero ->Bool
hayMovidasPosibles col (Tablero (n,m) pos)  = or [puedeJugarEn col t (f,c)  |f<-[0..(n-1)],c<-[0..(m-1)]]
	where t = (Tablero (n,m) pos)

--modificar col t coord = devuelve el tablero modificado luego de que juegue el color col en la coord. coord         
--Si se puede realizar la jugada, devuelve el tablero con las coordenadas invertidas y con el color en coord
--Si no, devuelve el mismo tablero.
--Para los 2 casos, el color devuelto es el del adversario.
modificar::Color->Tablero->Coordenada->(Color, Tablero)
modificar col t coord = (f col, tModif)	
	where 	tModif = Tablero (dim t) (nuevasPosic col (fColor t) (coord:(coordsQueInvierte col t coord )  ))
		f col = if (hayMovidasPosibles (negado col) tModif) 
		then (negado col )
		else if (hayMovidasPosibles col tModif) 	
		then col
		else col
	
--nuevasPosic col f xs (f,c) devuelve la nueva funcion que es la misma f pisando las coordenadas que estan en xs con el color col.
--Hace algo asi como dar los mismos colores que antes y el color col para las coordenadas que estan en xs.
nuevasPosic::Color->(Coordenada -> Maybe Color)->[Coordenada]->(Coordenada -> Maybe Color)
nuevasPosic col pos xs (f,c) 
	| elem (f,c) xs = Just col
	| otherwise = pos (f,c)

--Dado un juego devuelve el tablero del juego y el color al que le toca jugar.
tabCol :: Juego -> (Color, Tablero)
tabCol  j = foldJuego f1 f2 j
 	where	f2 (col, t) coord = modificar col t coord
		f1 dim = ( Negro, tableroInicial dim)

		
---------------------------------------------tablero -----------------------------------------
tablero :: Juego -> Tablero
tablero j = snd (tabCol j)

---------------------------------------------turno--------------------------------------
turno :: Juego -> Color
turno j | not( terminoElJuego j )= fst (tabCol j) 

---------------------------------------------movidasValidas------------------------------------

--Se fija en cada uno de las posiciones y de cada una de todas las posibles movidas para el turno actual en el tablero actual, arma un juego.
movidasValidas :: Juego -> [Juego]
movidasValidas j 
	| terminoElJuego j = []
	| otherwise = [ Poner j (f,c) | f<-[0..fst(dimension j)],c<-[0..snd(dimension j)], puedeJugarEn (turno j) (tablero j) (f,c)]

	
----------------------------------------------quienGano---------------------------------------
	
quienGano :: Juego -> Maybe Color
quienGano j | (diferenciaNegrasBlancas j) > 0 = Just Negro
			| (diferenciaNegrasBlancas j) < 0 = Just Blanco
			| (diferenciaNegrasBlancas j) == 0 = Nothing

-----------------------------------------------ultimaCoordJugada---------------------------
			
ultimaCoordJugada :: Juego -> Maybe Coordenada
ultimaCoordJugada = foldJuego (\x->Nothing) (\r c->Just c)

-----------------------------------------------diferenciaNegrasBlancas---------------------------

diferenciaNegrasBlancas :: Juego -> Int
diferenciaNegrasBlancas j = (contar Negro (tablero j )) - (contar Blanco ( tablero j ))
	where contar col (Tablero (n,m) pos) = length [(f, c) | f<-[0..n-1], c<-[0..m-1], pos( f, c) == Just col]

-------------------------------------------------Pruebas: De aca en adelante, es todo código para probar.
-- tabPrueba = Tablero (6,6) fPrueba
-- 
-- fPrueba::Coordenada->Maybe Color
-- fPrueba	coord 
-- 		| coord `elem` [(0,2),(0,3),(0,4),(1,0),(1,4),(2,1),(2,3),(3,2),(3,3),(4,0)] = Just Negro
-- 		| coord `elem` [(1,1),(1,2),(1,5),(2,2),(2,4),(3,1),(4,3),(5,3)] = Just Blanco
-- 		| otherwise = Nothing
-- 			
-- jPrueba = Poner(Poner (Comenzar (6,6)) (2,4))(3,4)
--       	    
-- 
-- instance Show Color where  
--   show Negro = "X"         
--   show Blanco = "O"        
-- 
-- mostrarColor :: Maybe Color -> String
-- mostrarColor = maybe "." show
-- 
-- mostrarResultado :: Maybe Color -> String
-- mostrarResultado =
--   maybe "Empatan."
--         (\ x -> "Ganan las " ++ show x ++ ".")
--   
-- instance Show Juego where
--   show j =
--     "\n" ++ lx ++ "\n" ++
--     concatMap mostrarFila [0..mx-1] ++
--     lx ++ "\n\n"
--     where
--       (Tablero (mx, my) pos) = tablero j
--       mostrarFila y =
--         " " ++ ly ++ marca (-1, y) ++
--         concat [mostrarColor (pos (y, x)) ++ marca (y, x) |
--                 x <- [0..my-1]] ++
--         ly ++ "\n"
--         where ly = show y
--       marca (x, y)
--         | ultimaCoordJugada j == Just (x, y) = ")"
--         | ultimaCoordJugada j == Just (x , y+1) = "("
--         | otherwise = " "
--       lx = "   " ++ concatMap (\ x -> (chr (ord '0' + x):" ")) [0..my-1]
-- 
-- instance Show Tablero where
--   show t =
--     "\n" ++ lx ++ "\n" ++
--     concatMap mostrarFila [0..mx-1] ++
--     lx ++ "\n\n"
--     where
--       (Tablero (mx, my) pos) = t
--       mostrarFila y =
--         " " ++ ly ++ " " ++
--         concat [mostrarColor (pos (y, x)) ++ " " |
--                 x <- [0..my-1]] ++
--         ly ++ "\n"
--         where ly = show y      
--       lx = "   " ++ concatMap (\ x -> (chr (ord '0' + x):" ")) [0..my-1]
-- 
-- -------------------------------------------------------------------------------
-- 
-- dimensionP = dimension jPrueba
-- 
-- pA1 = primerosAlineados Negro tabPrueba (1,3) --resultado esperado [(1,4),(2,3),(0,3),(0,4),(0,2),(1,0),(4,0)]
-- pA2 = primerosAlineados Blanco tabPrueba (1,3) --resultado esperado  [(1,2),(2,4),(2,2),(1,5),(4,3)]
-- 
-- dC1 = dameCoordEntre Blanco tabPrueba (1,3) (1,4) --resultado esperado  []
-- dC2 = dameCoordEntre Blanco tabPrueba (1,3) (2,3) --resultado esperado  []
-- dC3 = dameCoordEntre Blanco tabPrueba (1,3) (0,3) --resultado esperado  []
-- dC4 = dameCoordEntre Blanco tabPrueba (1,3) (0,4) --resultado esperado  []
-- dC5 = dameCoordEntre Blanco tabPrueba (1,3) (0,2) --resultado esperado  []
-- dC6 = dameCoordEntre Blanco tabPrueba (1,3) (1,0) --resultado esperado  [(1,1),(1,2)]
-- dC7 = dameCoordEntre Blanco tabPrueba (1,3) (4,0) --resultado esperado  [(2,2),(3,1)]
-- 
-- sC1 = sonConsecutivas (1,3) (1,0) dC6 --resultado esperado  True
-- sC2 = sonConsecutivas (1,3) (4,0) dC7 --resultado esperado  True
-- 
-- cE1 = coordEntre Negro tabPrueba (1,3) pA1 --resultado esperado [(1,1),(1,2),(2,2),(3,1)]
-- cE2 = coordEntre Blanco tabPrueba (1,3) pA2 --resultado esperado [(1,4),(2,3),(3,3)]


--------Pruebas  coordsQueInvierte 

-- cI1  = coordsQueInvierte Negro tabPrueba (5,2)	--resultado esperado  []
-- cI2  = coordsQueInvierte Blanco tabPrueba (5,2)	--resultado esperado  []
-- cI3  = coordsQueInvierte Negro tabPrueba (1,3)  --resultado esperado [(1,1),(1,2),(2,2),(3,1)]
-- cI4  = coordsQueInvierte Blanco tabPrueba (1,3) --resultado esperado [(1,4),(2,3),(3,3)]
-- cI5  = coordsQueInvierte Negro tabPrueba (6,3)  --resultado esperado  []
-- cI6  = coordsQueInvierte Negro tabPrueba (5,3)  --resultado esperado  [(4,3)]

-------Pruebas puedeJugarEn

-- pJEP1 = puedeJugarEn Negro tabPrueba (5,2)   --resultado esperado False
-- pJEP2 = puedeJugarEn Blanco tabPrueba (5,2)  --resultado esperado False
-- pJEP3 = puedeJugarEn Negro tabPrueba (1,3)   --resultado esperado True
-- pJEP4 = puedeJugarEn Blanco tabPrueba (1,3)  --resultado esperado True
-- pJEP5 = puedeJugarEn Negro tabPrueba (6,3)   --resultado esperado False
-- pJEP6 = puedeJugarEn Negro tabPrueba (5,3)	 --resultado esperado False

-------Pruebas ultimaCoordJugada

-- uCJ1 = ultimaCoordJugada jPrueba 	 --resultado esperado Just (3,4)
-- uCJ2 = ultimaCoordJugada ( Comenzar (6,6) ) --resultado esperado Nothing

-------Pruebas diferenciaNegrasBlancas

--	dnb1 = diferenciaNegrasBlancas j1
--	dnb2 = diferenciaNegrasBlancas j2

-------Pruebas quienGano

--	qg1 = quienGano j1 
--	qg2 = quienGano j2

-------Pruebas movidasValidas

--	mv1 = movidasValidas j1
--	mv2 = movidasValidas j2

-------Armado de juegos de pruebas varios
 
--NOTA , poner los ejemplos 

-- j1 = (Poner (Poner (Poner (Poner ( Poner (Comenzar (3,4)) (0,3)) (1,3))(2,1))(1,0)) (2,0) )
-- j2 = (Poner( Poner j2 (0,0))(2,3))
