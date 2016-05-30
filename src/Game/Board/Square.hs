module Game.Board.Square
(
    Square,
    col,


    newSquare,

    removeVal,
) where

    import Data.List

    import Game.Board.Value

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

