module Game.Board.Value
(
    Value,

    value,
) where

    data Value = Value
                { val  :: Int
                 }

    instance Show Value where
        show Value { val = v } = show v

    instance Eq Value where
        v == v' = (val v) == (val v')

    value :: Int -> Int -> Value
    value r v = Value v

