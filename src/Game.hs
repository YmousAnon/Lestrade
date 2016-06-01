module Game
(
    newGame,
) where

    import Control.Monad.Trans.State

    import Game.Board

    import System.Random


    data Game = Game
                { board    :: Board
                , solution :: Board
                }

    instance Show Game where
        show Game { board = b, solution = s } = show b++"\n"++show s
        --show Game { board = b, solution = s } = show b >> show "\n\n" >> show s

    newGame :: Int -> Int -> StdGen -> (Game,StdGen)
    newGame nR nC g = (Game
        { board    = b
        , solution = s
        },g')
        where
            b       =           newBoard       nR nC
            (s, g') = runState (genSolvedBoard nR nC) g
