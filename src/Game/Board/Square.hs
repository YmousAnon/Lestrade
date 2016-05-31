module Game.Board.Square
(
    Square,
    col,


    newSquare,

    removeVal,

    genSolvedSquare,
) where

    import Control.Monad.Trans.State

    import Data.List

    import Game.Board.Value

    import System.Random

    data Square = Solution
                { val  :: Value
                , col  :: Int
                 }
               | Alternatives
                { vals :: [Value]
                , col  :: Int
                 }
        deriving Show


    newSquare :: [Int] -> Int -> Int -> Square
    newSquare [v] r c = Solution
        { val  = (newValue r) v
        , col  = c
        }
    newSquare vs r c = Alternatives
        { vals = map (newValue r) vs
        , col  = c
        }


    removeVal :: Value -> Square -> Square
    removeVal v p = Alternatives
        { vals = delete v $ vals p
        , col  = col             p
        }



    genSolvedSquare :: Int -> Int -> State ([Int],StdGen) Square
    genSolvedSquare r nC = state $ \(vs,g) ->
        let ig     = randomR (0, length vs-1) g
            (v,g') = (vs !! fst ig, snd ig)
            c      = nC - length vs
        in  (newSquare [v] r c, (delete v vs, g'))
