module Game
(
    Game,
    gameInit,
    gameTimeStep,
) where

    import Control.Monad
    import Control.Monad.Trans.State

    import Data.List
    import Data.IORef

    import Game.Board
    import Game.HintBoard
    import Game.HintBoard.Hint
    import Game.HintBoard.Vertical
    import Game.HintBoard.Horizontal
    import Game.SolutionState
    import Game.SolutionState.Loss
    import Game.SolutionState.Victory

    import UI
    import UI.Coordinate
    import UI.Input
    import UI.Input.Settings
    import UI.Render

    import System.Environment (getArgs)
    import System.Random


    data Game = Game
                { board    :: Board
                , solution :: Board
                , vhb      :: HintBoard
                , hhb      :: HintBoard
                , area     :: Area
                , loss     :: Loss
                , victory  :: Victory
                }

    instance Renderable Game where
        render w g = render                w (board   g)
                  >> render                w (vhb     g)
                  >> render                w (hhb     g)
                  >> whenLoss    g (render w (loss    g))
                  >> whenVictory g (render w (victory g))
        getArea    = area

    instance Clickable Game where
        lclick ui pt g = do
            let lClickUnSolved x = ifUnSolved g (lclick ui pt) x
            b'   <- lClickUnSolved (board g)
            vhb' <- lClickUnSolved (vhb   g)
            hhb' <- lClickUnSolved (hhb   g)
            return Game
                   { board    = b'
                   , solution = solution g
                   , vhb      = vhb'
                   , hhb      = hhb'
                   , area     = area     g
                   , loss     = loss     g
                   , victory  = victory  g
                   }
        rclick ui pt g = do
            let rClickUnSolved x = ifUnSolved g (rclick ui pt) x
            b'   <- rClickUnSolved (board g)
            vhb' <- rClickUnSolved (vhb   g)
            hhb' <- rClickUnSolved (hhb   g)
            return Game
                   { board    = b'
                   , solution = solution g
                   , vhb      = vhb'
                   , hhb      = hhb'
                   , area     = area     g
                   , loss     = loss     g
                   , victory  = victory  g
                   }



    gameInit :: Int -> IO Game
    gameInit seed = do
        putStrLn ("seed: "++show seed)
        let g = mkStdGen seed

        nC  <- read <$> getSetting "columns"
        nR  <- read <$> getSetting "rows"

        hPS <- read <$> getSetting "hintsPerSquare"

        iS  <- read <$> getSetting "initialSolved"

        wBW <- read <$> getSetting "windowBorderWidth"
        pD  <- read <$> getSetting "paneDistance" :: IO Int

        b   <- newBoard nR nC (wBW,wBW)

        let (s ,(_,g' )) = runState (genSolution nR nC) ([1..nC],g)
        s' <- s
        let (b',(_,g'')) = runState (initialSol iS b s') (concat [[(r,c)
                                                          | r <- [0..nR-1]]
                                                          | c <- [0..nC-1]]
                                                         ,g')
        let y = getYMax $ getArea b'
            x = getXMax $ getArea b'


        let is          = concat [[(r,c) | r <- [0..nR-1]] | c <- [0..nC-1]]
            nHints      = round (hPS*fromIntegral (nC*nR))
            (his ,g''') = runState (genHintList s' True =<<
                                    scrambleIndices nHints is) g''

        is' <- flip removeContained is . snd <$> his
        let (his',g'''') = runState (genHintList s' False is') g'
            hs = removeDuplicates . sort <$> ((++) <$> (fst <$> his)
                                                   <*> (fst <$> his'))
            --hs =                    sort <$> ((++) <$> (fst <$> his)
            --hs =                    sort <$> (fst <$> his')


        putStrLn ""
        putStrLn ""
        putStrLn . show . snd =<< his
        putStrLn ""
        putStrLn ""
        putStrLn $ show is'
        putStrLn ""
        putStrLn ""

        --putStrLn ""
        --print . snd =<< his
        --putStrLn ""
        --print is'
        --putStrLn ""
        --print . snd =<< his'

        hhb' <- let ym    = (getYMax $ getArea b')
                    hhb'' = newEmptyHintBoard (x+pD,wBW) ym Horizontal
                 in hs  >>= addHintList hhb'' >>= fillHintBoard
        vhb' <- let xm    = (getXMax $ getArea hhb')
                    vhb'' = newEmptyHintBoard (wBW,y+pD) xm Vertical
                 in hs  >>= addHintList vhb'' >>= fillHintBoard
        print s'
        let a = uncurry (newArea (0,0))
              $ (wBW,wBW)>+<getAreaEnd (getArea b'\/getArea vhb'\/getArea hhb')
        v <-  newVictory s' a g''''
        return Game
            { board    = b'
            , solution = s'
            , vhb      = vhb'
            , hhb      = hhb'
            , area     = a
            , loss     = newLoss a
            , victory  = v
            }

    gameTimeStep :: UI -> Game -> IO Game
    gameTimeStep s g = do victory' <- ifSolved g updateVictory $ victory g
                          loss'    <- ifLoss   g updateLoss    $ loss    g
                          whenVictory g (dirtyWindow s)
                          return Game
                                 { board    = board    g
                                 , solution = solution g
                                 , vhb      = vhb      g
                                 , hhb      = hhb      g
                                 , area     = area     g
                                 , loss     = loss'
                                 , victory  = victory'
                                 }



    ifSolved :: Game -> (a -> IO a) -> a -> IO a
    ifSolved g act = if (board g|-|solution g)==Correct then act else return

    ifLoss :: Game -> (a -> IO a) -> a -> IO a
    ifLoss g act = if (board g|-|solution g)==Wrong then act else return

    ifUnSolved :: Game -> (a -> IO a) -> a -> IO a
    ifUnSolved g act = if (board g|-|solution g)==UnSolved then act else return

    whenLoss :: Game -> IO() -> IO()
    whenLoss g = when ((board g|-|solution g)==Wrong)

    whenVictory :: Game -> IO() -> IO()
    whenVictory g = when ((board g|-|solution g)==Correct)
