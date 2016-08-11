module Game
(
    Game,
    gameInit,
) where

    import Control.Monad.Trans.State

    import Data.IORef

    import Game.Board
    import Game.HintBoard
    import Game.HintBoard.Hint
    import Game.HintBoard.Vertical
    import Game.HintBoard.Horizontal

    import Interface.Input
    import Interface.Input.Settings
    import Interface.Render

    import System.Environment (getArgs)
    import System.Random

    import Interface.Coordinate

    data Game = Game
                { board    :: Board
                , solution :: Board
                , vhb      :: HintBoard
                , hhb      :: HintBoard
                , area     :: Area
                }

    instance Renderable Game where
        render w g = render w (board g)
                  >> render w (vhb   g)
                  >> render w (hhb   g)
        getArea    = area
        --(getArea (board g)
        --           \/ getArea (vhb   g)
        --           \/ getArea (hhb   g))
        --           >+<

    instance Clickable Game where
        lclick pt g = do b'   <- lclick pt $ board g
                         vhb' <- lclick pt $ vhb   g
                         hhb' <- lclick pt $ hhb   g
                         return Game
                             { board    = b'
                             , solution = solution g
                             , vhb      = vhb'
                             , hhb      = hhb'
                             , area     = area     g
                             }
        rclick pt g = do b'   <- rclick pt $ board g
                         vhb' <- rclick pt $ vhb   g
                         hhb' <- rclick pt $ hhb   g
                         return Game
                             { board    = b'
                             , solution = solution g
                             , vhb      = vhb'
                             , hhb      = hhb'
                             , area     = area     g
                             }



    gameInit :: Int -> IO Game
    gameInit seed = do
        let g = mkStdGen seed

        nC  <- read <$> getSetting "columns"
        nR  <- read <$> getSetting "rows"

        hPS <- read <$> getSetting "hintsPerSquare"

        iS  <- read <$> getSetting "initialSolved"

        wBW <- read <$> getSetting "windowBorderWidth"
        pD  <- read <$> getSetting "paneDistance"

        b   <- newBoard nR nC (wBW,wBW)

        let (s, (_, g'))    = runState (genSolution nR nC) ([1..nC],g)
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
        let nHints = round (hPS*fromIntegral (nC*nR))
            (hs,g''') = runState (genHintList s' =<< (scrambleIndices nHints
                                 (concat [[(r,c)
                                 | r <- [0..nR-1]]
                                 | c <- [0..nC-1]])
                                 )) g'




        vhb' <- let xm    = (getXMax $ getArea b')
                    vhb'' = newEmptyHintBoard (wBW,y+pD) xm Vertical
                  in hs >>= addHintList vhb'' >>= fillHintBoard
        hhb' <- let ym    = (getYMax $ getArea vhb')
                    hhb'' = newEmptyHintBoard (x+pD,wBW) ym Horizontal
                  in hs >>= addHintList hhb'' >>= fillHintBoard

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
                (getAreaEnd (getArea b'\/getArea vhb'\/getArea hhb'))
        return Game
            { board    = b'
            , solution = s'
            , vhb      = vhb'
            , hhb      = hhb'
            , area     = a
            }
