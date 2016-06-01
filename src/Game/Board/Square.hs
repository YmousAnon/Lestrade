module Game.Board.Square
(
    Square,
    col,

    square,
    genSolvedSquare,

    removeVal,

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

    instance Show Square where
        show Solution     { val  = v } = show v
        show Alternatives { vals = v } = show v


    square :: [Int] -> Int -> Int -> Square
    square [v] r c = Solution
        { val  = (value r) v
        , col  = c
        }
    square vs r c = Alternatives
        { vals = map (value r) vs
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
        in  (square [v] r c, (delete v vs, g'))
