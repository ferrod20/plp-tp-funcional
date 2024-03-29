module IA(Estrategia, estrategiaIA) where

import Reversi
import Minimax

type Estrategia = Juego -> Juego

estrategiaIA :: Estrategia
estrategiaIA j = minimax diferenciaNegrasBlancas (\ju-> (turno ju) == Negro) (podar 5 (arbolDeMovidas movidasValidas j ))

---Pruebas
p1 = estrategiaIA (Comenzar(4,4))
p2 = estrategiaIA (Poner (Comenzar(4,4)) (0,2))