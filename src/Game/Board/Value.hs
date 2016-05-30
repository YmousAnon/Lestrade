module Game.Board.Value
(
    Value,

    newValue,
) where

    data Value = Value
                { val  :: Int
                 }
        deriving Show

    instance Eq Value where
        v == v' = (val v) == (val v')

    newValue :: Int -> Int -> Value
    newValue r v = Value v

