module Reversi
(
	Juego(..), Color(..), Dimension, Coordenada, Tablero(..),
	turno, movidasValidas, enRango, dimension, tablero,
	puedeJugarEn, quienGano, ultimaCoordJugada, terminoElJuego,
	diferenciaNegrasBlancas
) 

where

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

enRango :: Coordenada -> Dimension -> Bool
enRango   (a,b) (c,d) = a<c && b<d && a>=0 && b>=0

foldJuego :: (Dimension->b)->(b->Coordenada->b)->Juego->b
foldJuego f1 f2 (Comenzar d) = f1 d
foldJuego f1 f2 (Poner j c ) = f2 (foldJuego f1 f2 j) c

dimension :: Juego -> Dimension
dimension = foldJuego id  f2
	where f2 r c = r

-------------------------------------------------------------------------------  coordsQueInvierte 
coordsQueInvierte :: Color -> Tablero -> Coordenada -> [Coordenada]
coordsQueInvierte col t c = coordEntre c (primerosAlineados col t c ) 

--primerosAlineados k t c devuelve todos los puntos de color k mas cercanos a c que estan alineados ortogonalemente o diagonalmente al punto c en el tablero t
primerosAlineados:: Color -> Tablero -> Coordenada -> [Coordenada]
primerosAlineados k (Tablero (n,m) posic) (f,c) =  filter (masCercano puntos (f,c)) puntos
	where puntos = [ a | i<-[0..(max (n-1) (m-1))], a<-[(f,c+i),(f,c-i),(f+i,c),(f-i,c),(f+i,c+i),(f+i,c-i),(f-i,c+i),(f-i,c-i)], enRango a (n,m) && (posic a) == Just k ]			

--masCercano xs t c p dice si c es el punto mas cercano de todos los puntos xs al punto p
masCercano::[Coordenada]->Coordenada->Coordenada->Bool
masCercano xs p c = foldr (f c p) True xs
	where f c p x r = (masCerca c x p ) && r
--	
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
coordEntre::Coordenada->[Coordenada]->[Coordenada]
coordEntre c xs = foldr (f c) [] xs
	where f c x r = ( dameCoordEntre c x ) ++ r

--dameCoordEntre d h devuelve todas las coordenadas lineales entre d y h.
dameCoordEntre::Coordenada->Coordenada->[Coordenada]
dameCoordEntre (df,dc) (hf,hc) = [(f,c)| f<-[(min df hf)..(max df hf)], c<-[(min dc hc)..(max dc hc)], ( hc == dc || hf == df || abs(hc-c)==abs(hf-f) ) && (f,c) /= (df, dc) && (f,c) /= (hf, hc)]

negado Blanco = Negro
negado Negro = Blanco

-------------------------------------------------------------------------------  coordsQueInvierte 
puedeJugarEn :: Color -> Tablero -> Coordenada -> Bool
puedeJugarEn k (Tablero d pos) c = (enRango c d) && ((pos c) == Nothing) && length( coordsQueInvierte k t c ) > 0
	where t = (Tablero d pos)

terminoElJuego :: Juego -> Bool
terminoElJuego j = and [not(algunoJuega j f c)  |f<-[0..fst(dimension j)],c<-[0..snd(dimension j)]]
	where algunoJuega j f c = puedeJugarEn Blanco (tablero j) (f,c) || puedeJugarEn Negro (tablero j) (f,c)

turno :: Juego -> Color
turno j | not( terminoElJuego j )= fst (tabCol j)
         
--modificar col t coord = devuelve el tablero modificado luego de que juegue el color col en la coord. coord         
modificar::Color->Tablero->Coordenada->(Color, Tablero)
modificar col (Tablero dim pos) coord 
	| puedeJugarEn col t coord = (negado col, Tablero dim (nuevasPosic col (pos) (coordsQueInvierte col t coord )))
	| otherwise = (col, t)
		where t =(Tablero dim pos)		
 
nuevasPosic::Color->(Coordenada -> Maybe Color)->[Coordenada]->(Coordenada -> Maybe Color)
nuevasPosic col pos xs (f,c) 
	| elem (f,c) xs = Just (negado col)
	| otherwise = pos (f,c)

--Devuelve el tablero y el color que tiene que jugar
tabCol :: Juego -> (Color, Tablero)
tabCol  j = foldJuego f1 f j
 	where	f (col, t) coord = modificar col t coord
		f1 dim = (Blanco, tableroInicial dim)

tablero :: Juego -> Tablero
tablero j = snd (tabCol j)

movidasValidas :: Juego -> [Juego]
movidasValidas j 
	| terminoElJuego j = j:[]
	| otherwise = [ Poner j (f,c) | f<-[0..fst(dimension j)],c<-[0..snd(dimension j)], puedeJugarEn (turno j) (tablero j) (f,c)]

quienGano :: Juego -> Maybe Color
quienGano j | (diferenciaNegrasBlancas j) > 0 = Just Negro
			| (diferenciaNegrasBlancas j) < 0 = Just Blanco
			| (diferenciaNegrasBlancas j) == 0 = Nothing

ultimaCoordJugada :: Juego -> Maybe Coordenada
ultimaCoordJugada = foldJuego (\x->Nothing) (\r c->Just c)

diferenciaNegrasBlancas :: Juego -> Int
diferenciaNegrasBlancas j = (contar Negro (tablero j )) - (contar Blanco ( tablero j ))
	where contar col (Tablero (n,m) pos) = length [(f, c) | f<-[0..n-1], c<-[0..m-1], pos( f, c) == Just col]



-------------------------------------------------Pruebas
tabPrueba = Tablero (6,6) pos
	where pos coord
		| coord `elem` [(0,2),(0,3),(0,4),(1,0),(1,4),(2,1),(2,3),(3,2),(3,3),(4,0)] = Just Negro
		| coord `elem` [(1,1),(1,2),(1,5),(2,2),(2,4),(3,1),(4,3),(5,3)] = Just Blanco
		| otherwise = Nothing

juegoPrueba = Poner (Comenzar (6,6)) (4,3)
inicial = tableroInicial ( 6,6 )
      	    
dim::Tablero->Dimension
dim (Tablero d f) = d

color::Tablero->Coordenada->Maybe Color
color (Tablero d f) c = f c

instance Show Color where  
  show Negro = "X"         
  show Blanco = "O"        
-------------------------------------------------------------------------------

dimensionP = dimension juegoPrueba
primerosAlineadosP = primerosAlineados Negro tabPrueba (1,3)
coordsQueInvierteP1 = coordsQueInvierte Negro tabPrueba (1,3)
coordsQueInvierteP2 = coordsQueInvierte Blanco tabPrueba (1,3)

puedeJugarEnP1 = puedeJugarEn Negro tabPrueba (5,2)
puedeJugarEnP2 = puedeJugarEn Blanco tabPrueba (5,2)
puedeJugarEnP3 = puedeJugarEn Negro tabPrueba (1,3)
puedeJugarEnP4 = puedeJugarEn Blanco tabPrueba (1,3)
puedeJugarEnP5 = puedeJugarEn Negro tabPrueba (6,3)
puedeJugarEnP6 = puedeJugarEn Negro tabPrueba (5,3)

cI1  = coordsQueInvierte Negro tabPrueba (5,2)
cI2  = coordsQueInvierte Blanco tabPrueba (5,2)
cI3  = coordsQueInvierte Negro tabPrueba (1,3)
cI4  = coordsQueInvierte Blanco tabPrueba (1,3)
cI5  = coordsQueInvierte Negro tabPrueba (6,3)
cI6  = coordsQueInvierte Negro tabPrueba (5,3)
