module IA(estrategiaIA) where

import Reversi
import Minimax

estrategiaIA :: Juego -> Juego
estrategiaIA j = minimax diferenciaNegrasBlancas (\ju-> (turno ju) == Negro) (podar 5 (arbolDeMovidas movidasValidas j ))

