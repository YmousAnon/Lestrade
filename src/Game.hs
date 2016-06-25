module Game
(
    gameInit,
) where

    import Control.Monad.Trans.State

    import Data.IORef

    import Game.Board

    import Interface.Render

    import Settings

    import System.Environment (getArgs)
    import System.Random


    data Game = Game
                { board    :: Board
                , solution :: Board
                , gen      :: StdGen
                }

    instance Renderable Game where
        render Game { board = b } = render b


    gameInit :: IO (IORef Game)
    gameInit = do
        g  <- mkStdGen <$> read <$> head <$> getArgs

        nC <-              read <$> getVal "columns"
        nR <-              read <$> getVal "rows"

        let (s,g') = runState (solvedBoard nR nC) g
            b      =           newBoard    nR nC
         in Game
                { board    = b
                , solution = s
                , gen      = g'
                }
