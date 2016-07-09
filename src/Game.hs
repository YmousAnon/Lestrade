module Game
(
    gameInit,
) where

    import Control.Monad.Trans.State

    import Data.IORef

    import Game.Board
    import Game.Hints.Vertical
    import Game.Hints.Horizontal

    import Interface.Input
    import Interface.Render

    import Settings

    import System.Environment (getArgs)
    import System.Random

    import Interface.Coordinate

    data Game = Game
                { board    :: Board
                , solution :: Board
                , gen      :: StdGen
                , vhb      :: VHintBoard
                , hhb      :: HHintBoard
                }

    instance Renderable Game where
        --render w g = mapM_ (render w) $ (map ($ g) [board, vhb]
        render w g = render w (board g)
                  >> render w (vhb   g)
                  >> render w (hhb   g)
        --getArea  g = mapM_ getArea  $ map ($ g) [board, vhb]
        getArea  g = getArea (board g)
                  \/ getArea (vhb   g)
                  \/ getArea (hhb   g)
                   -- \/ getArea (newVHint [] (100,100))

    instance Clickable Game where
        lclick pt g = do b <- lclick pt $ board    g
                         return Game
                             { board    = b
                             , solution = solution g
                             , gen      = gen      g
                             , vhb      = vhb      g
                             , hhb      = hhb      g
                             }
        rclick pt g = do b <- rclick pt $ board    g
                         return Game
                             { board    = b
                             , solution = solution g
                             , gen      = gen      g
                             , vhb      = vhb      g
                             , hhb      = hhb      g
                             }



    --updateBoard :: Game -> Board -> Game
    --updateBoard g b = Game
    --    { board    = b
    --    , solution = solution g
    --    , gen      = gen      g
    --    }

    gameInit :: IO (IORef Game)
    gameInit = do
        g  <- (mkStdGen . read . head) <$> getArgs

        nC <- read <$> getSetting "columns"
        nR <- read <$> getSetting "rows"

        is <- read <$> getSetting "initialSolved"

        b  <- newBoard nR nC (0,0)

        --vH <- emptyHintPanel (0,getYMax $ getArea b)

        let (s, (_, g'))    = runState (genSolution nR nC) ([1..nC],g)
        do s' <- s
           print s'
           let (b',(_,g'')) = runState (initialSol is b s') (concat [[(r,c)
                                                            | r <- [0..nR-1]]
                                                            | c <- [0..nC-1]]
                                                            ,g')
           let y = getYMax $ getArea b'
               x = getXMax $ getArea b'
           vhb <- fillVHintBoard $ newEmptyVHintBoard (0,y) (getXMax $ getArea b')
           hhb <- fillHHintBoard $ newEmptyHHintBoard (x,0) (getYMax $ getArea b')
           newIORef Game
               { board    = b'
               , solution = s'
               , gen      = g
               , vhb      = vhb
               , hhb      = hhb
               }
