module Game
(
    gameInit,
) where

    import Control.Monad.Trans.State

    import Data.IORef

    import Game.Board

    import Interface.Input
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
        render w g = render w      $ board g
        getArea  g = getArea       $ board g

    instance Clickable Game where
        lclick pt g = do b <- lclick pt $ board    g
                         return Game
                             { board    = b
                             , solution = solution g
                             , gen      = gen      g
                             }
        rclick pt g = do b <- rclick pt $ board    g
                         return Game
                             { board    = b
                             , solution = solution g
                             , gen      = gen      g
                             }



    updateBoard :: Game -> Board -> Game
    updateBoard g b = Game
        { board    = b
        , solution = solution g
        , gen      = gen      g
        }

    gameInit :: IO (IORef Game)
    gameInit = do
        g  <- (mkStdGen . read . head) <$> getArgs

        nC <- read <$> getSetting "columns"
        nR <- read <$> getSetting "rows"

        is <- read <$> getSetting "initialSolved"

        b  <- newBoard    nR nC (0,0)
        let (s, (_, g'))    = runState (genSolution nR nC) ([1..nC],g)
        do s' <- s
           let (b',(_,g'')) = runState (initialSol is b s') (concat [[(r,c)
                                                            | r <- [0..nR-1]]
                                                            | c <- [0..nC-1]]
                                                            ,g')
           print $ evalState (initialSol' is) (concat [[(r,c) | r <- [0..nR-1]]
                                                              | c <- [0..nC-1]]
                                                              ,g')
           print s'
           newIORef Game
               --{ board    = b
               { board    = b'
               , solution = s'
               , gen      = g
               }
