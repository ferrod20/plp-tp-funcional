module Main where

import Reversi
import IA
import Jugar

tamTablero :: Dimension
tamTablero = (4, 4)

mainHumanoContraIA = jugarHumanoContraIA estrategiaIA (Comenzar tamTablero)
mainHumanoContraHumano = jugarHumanoContraHumano $ Comenzar tamTablero
mainIAContraIA = jugarIAContraIA estrategiaIA estrategiaIA (Comenzar tamTablero)

main = mainHumanoContraIA
