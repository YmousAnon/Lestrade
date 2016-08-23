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

    import Interface.Input
    import Interface.Input.Settings
    import Interface.Render
    import Interface.Screen

    import System.Environment (getArgs)
    import System.Random

    import Interface.Coordinate


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
        lclick pt g = do let lClickNotCorrect x = ifNotCorrect g (lclick pt) x
                         b'   <- lClickNotCorrect (board g)
                         vhb' <- lClickNotCorrect (vhb   g)
                         hhb' <- lClickNotCorrect (hhb   g)
                         return Game
                                { board    = b'
                                , solution = solution g
                                , vhb      = vhb'
                                , hhb      = hhb'
                                , area     = area     g
                                , loss     = loss     g
                                , victory  = victory  g
                                }
        rclick pt g = do let rClickNotCorrect x = ifNotCorrect g (rclick pt) x
                         b'   <- rClickNotCorrect (board g)
                         vhb' <- rClickNotCorrect (vhb   g)
                         hhb' <- rClickNotCorrect (hhb   g)
                         return Game
                                { board    = b'
                                , solution = solution g
                                , vhb      = vhb'
                                , hhb      = hhb'
                                , area     = area     g
                                , loss     = loss     g
                                , victory  = victory  g
                                }

    ifSolved :: Game -> (a -> IO a) -> a -> IO a
    ifSolved g act = if (board g|-|solution g)==Correct then act else return

    ifNotCorrect :: Game -> (a -> IO a) -> a -> IO a
    ifNotCorrect g act = if (board g|-|solution g)/=Correct then act
                                                            else return

    whenLoss :: Game -> IO() -> IO()
    whenLoss g = when ((board g|-|solution g)==Wrong)

    whenVictory :: Game -> IO() -> IO()
    whenVictory g = when ((board g|-|solution g)==Correct)



    gameInit :: Int -> IO Game
    gameInit seed = do
        let g = mkStdGen seed

        nC  <- read <$> getSetting "columns"
        nR  <- read <$> getSetting "rows"

        hPS <- read <$> getSetting "hintsPerSquare"

        iS  <- read <$> getSetting "initialSolved"

        wBW <- read <$> getSetting "windowBorderWidth"
        pD  <- read <$> getSetting "paneDistance" :: IO Int

        b   <- newBoard nR nC (wBW,wBW)

        let (s, (_, g')) = runState (genSolution nR nC) ([1..nC],g)
        s' <- s
        let (b',(_,g'')) = runState (initialSol iS b s') (concat [[(r,c)
                                                          | r <- [0..nR-1]]
                                                          | c <- [0..nC-1]]
                                                         ,g')
        let y = getYMax $ getArea b'
            x = getXMax $ getArea b'


        -- !==! --
        --let (ioVHint1,g''' ) = runState (genVHint (0,0) s') g''
        --    (ioVHint2,g'''') = runState (genVHint (0,0) s') g'''
        --vhint1 <- ioVHint1
        --vhint2 <- ioVHint2


        --hints <- scrambleIndices []
        --let (b',g'') = genHintList s 10 <$> runState scrambleIndices
        --let (hs,g'') = runState (genHintList s 10 <$> scrambleIndices
        --let (hs,g'') = runState (genHintList s' 10 =<< (scrambleIndices
        let is  = concat [[(r,c) | r <- [0..nR-1]] | c <- [0..nC-1]]
            nHints = round (hPS*fromIntegral (nC*nR))
            (his ,g''') = runState (genHintList s' =<< scrambleIndices nHints
                                    is) g''

        is' <- flip removeContained is . removeDuplicates . snd <$> his
        --print ""
        --print . snd =<< his
        --print ""
        --print is'
        --print ""
        let (his',g'''') = runState (genHintList s' is') g'
            --hs = sort . removeDuplicates . concat <$> (fst <$> his)
            --                                      <*> (fst <$> his)
            hs = sort . removeDuplicates <$> ((++) <$> (fst <$> his)
                                                   <*> (fst <$> his))
            --hs = sort . removeDuplicates . (++) <$> (fst <$> his)
            --                                    <*> (fst <$> his)


        hhb' <- let ym    = (getYMax $ getArea b')
                    hhb'' = newEmptyHintBoard (x+pD,wBW) ym Horizontal
                 in hs  >>= addHintList hhb'' >>= fillHintBoard
        vhb' <- let xm    = (getXMax $ getArea hhb')
                    vhb'' = newEmptyHintBoard (wBW,y+pD) xm Vertical
                 in hs  >>= addHintList vhb'' >>= fillHintBoard

        --let (ioHHint1,g''' ) = runState (genHHint (1,1) s') g''
        --hhint1 <- ioHHint1

        --vhb'  <- fillHintBoard =<< addHintList [vhint1,vhint2] emptyVBoard --addHint vhint2 =<< addHint vhint1 emptyHBoard
        ---- !==! --

        ----print (getYMax $ getArea b')
        --hhb' <- fillHintBoard =<< addHintList [hhint1,vhint1,vhint2] emptyHBoard

        -- !==! --



        --(addHint (newEmptyHintBoard (0,y) (getXMax $ getArea b')))


        --vhb <- fillHintBoard =<< (addHint (newEmptyHintBoard (0,y) (getXMax $ getArea b')) =<< evalState (genHint s') (us,g''))


        --vhb <- fillHintBoard $ newEmptyHintBoard (0,y) (getXMax $ getArea b') Vertical
        --vhb <- fillHintBoard =<< (addHint (newEmptyHintBoard (0,y) (getXMax $ getArea b')) =<< evalState (genHint s') (us,g''))
        --vhb <- fillHintBoard =<< (addVHint (newEmptyHintBoard (0,y) (getXMax $ getArea b')) =<< evalState (genHint s') (us,g''))
        --hhb <- fillHHintBoard $ newEmptyHHintBoard (x,0) (getYMax $ getArea b')
        --print $ evalState (genHint s') (us,g'')
        --print =<<
        --print us
        --print b'
        --print b
        print s'
        --print a
        let a = uncurry (newArea (0,0)) $ (wBW,wBW) >+<
                getAreaEnd (getArea b'\/getArea vhb'\/getArea hhb')
        v <-  newVictory s' a g''''
        --v <- updateVictory =<< updateVictory =<< newVictory s' a g'''
        return Game
            { board    = b'
            , solution = s'
            , vhb      = vhb'
            , hhb      = hhb'
            , area     = a
            , loss     = newLoss a
            , victory  = v
            }

    gameTimeStep :: Screen -> Game -> IO Game
    gameTimeStep s g = do victory' <- ifSolved g updateVictory $ victory g
                          dirtyScreen s
                          return Game
                                 { board    = board    g
                                 , solution = solution g
                                 , vhb      = vhb      g
                                 , hhb      = hhb      g
                                 , area     = area     g
                                 , loss     = loss     g
                                 , victory  = victory'
                                 }
