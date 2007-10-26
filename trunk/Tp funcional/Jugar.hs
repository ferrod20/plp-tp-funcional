module Jugar(
  jugarIAContraIA, jugarHumanoContraHumano, jugarHumanoContraIA
) where

import Char
import Reversi
import IA

type Jugador = Juego -> IO Juego

mostrarColor :: Maybe Color -> String
mostrarColor = maybe "." show

mostrarResultado :: Maybe Color -> String
mostrarResultado =
  maybe "Empatan."
        (\ x -> "Ganan las " ++ show x ++ ".")

instance Show Color where
  show Negro = "X"
  show Blanco = "O"

instance Show Juego where
  show j =
    "\n" ++ lx ++ "\n" ++
    concatMap mostrarFila [0..my-1] ++
    lx ++ "\n\n"
    where
      (Tablero (mx, my) pos) = tablero j
      mostrarFila y =
        " " ++ ly ++ marca (-1, y) ++
        concat [mostrarColor (pos (x, y)) ++ marca (x, y) |
                x <- [0..mx-1]] ++
        ly ++ "\n"
        where ly = show y
      marca (x, y)
        | ultimaCoordJugada j == Just (x, y) = ")"
        | ultimaCoordJugada j == Just (x + 1, y) = "("
        | otherwise = " "
      lx = "   " ++ concatMap (\ x -> (chr (ord 'A' + x):" ")) [0..mx-1]

parsear :: String -> Dimension -> Maybe Coordenada
parsear [x1, x2] dim
  | enRango (c1, c2) dim = Just (c1, c2)
  | otherwise = Nothing
  where
    c1 = x1 `dif` if isUpper x1 then 'A' else 'a'
    c2 = x2 `dif` '0'
    dif x a = ord(x) - ord(a)
parsear _ _ = Nothing

jugadorHumano :: Jugador
jugadorHumano j = do
    putStr "Coordenadas (letra+numero -- ej. A1):\n"
    comando <- getLine
    ejecutar $ minusculas comando
  where
    minusculas = map toLower
    ejecutar cmd =
       case coord cmd of
         (Just c) ->
           if puedeJugar c
             then jugar c
             else insistir
         Nothing ->
           insistir
    coord cmd = parsear cmd $ dimension j
    puedeJugar c = puedeJugarEn (turno j) (tablero j) c
    jugar c = return $ Poner j c
    insistir = jugadorHumano j

jugadorIA :: Estrategia -> Jugador
jugadorIA proximaMovida j = do
  putStr "Pensando...\n"
  return $ proximaMovida j

jugar :: Jugador -> Jugador -> Juego -> IO ()
jugar jugador1 jugador2 juego = do
  putStr $ show juego
  if terminoElJuego juego
   then do
     putStr "Termino el juego.\n"
     putStr $ mostrarResultado $ quienGano juego
     putStr "\n\n"
   else do
     juego <- jugador (turno juego) juego
     jugar jugador1 jugador2 juego
  where
    jugador Negro = jugador1
    jugador Blanco = jugador2

jugarHumanoContraHumano :: Juego -> IO ()
jugarHumanoContraHumano = jugar jugadorHumano jugadorHumano

jugarHumanoContraIA :: Estrategia -> Juego -> IO ()
jugarHumanoContraIA e = jugar jugadorHumano (jugadorIA e)

jugarIAContraIA :: Estrategia -> Estrategia -> Juego -> IO ()
jugarIAContraIA e1 e2 = jugar (jugadorIA e1) (jugadorIA e2)

