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
        render  window = render  window . board
        getArea        = getArea . board


    gameInit :: IO (IORef Game)
    gameInit = do
        g  <- (mkStdGen . read . head) <$> getArgs

        nC <- read <$> getSetting "columns"
        nR <- read <$> getSetting "rows"

        b  <- newBoard    nR nC (0,0)
        let --(s,g') = runState (solvedBoard nR nC) g
         in newIORef Game
                { board    = b
                , gen      = g
                }
                --{ board    = b
                --, solution = s
                --, gen      = g'
                --}
