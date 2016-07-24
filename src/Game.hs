module Game
(
    Game,
    gameInit,
) where

    import Control.Monad.Trans.State

    import Data.IORef

    import Game.Board
    import Game.Hints
    import Game.Hints.Vertical
    import Game.Hints.Horizontal

    import Interface.Input
    import Interface.Input.Settings
    import Interface.Render

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
        render w g = render w (board g)
                  >> render w (vhb   g)
                  >> render w (hhb   g)
        getArea  g = getArea (board g)
                  \/ getArea (vhb   g)
                  \/ getArea (hhb   g)

    instance Clickable Game where
        lclick pt g = do b'   <- lclick pt $ board    g
                         vhb' <- lclick pt $ vhb      g
                         return Game
                             { board    = b'
                             , solution = solution g
                             , gen      = gen      g
                             , vhb      = vhb'
                             , hhb      = hhb      g
                             }
        rclick pt g = do b'   <- rclick pt $ board    g
                         vhb' <- rclick pt $ vhb      g
                         return Game
                             { board    = b'
                             , solution = solution g
                             , gen      = gen      g
                             , vhb      = vhb'
                             , hhb      = hhb      g
                             }



    gameInit :: Int -> IO Game
    gameInit seed = do
        let g = mkStdGen seed

        nC <- read <$> getSetting "columns"
        nR <- read <$> getSetting "rows"

        is <- read <$> getSetting "initialSolved"

        b  <- newBoard nR nC (0,0)

        let (s, (_, g'))    = runState (genSolution nR nC) ([1..nC],g)
        do s' <- s
           let (b',(us,g'')) = runState (initialSol is b s') (concat [[(r,c)
                                                             | r <- [0..nR-1]]
                                                             | c <- [0..nC-1]]
                                                             ,g')
           let y = getYMax $ getArea b'
               x = getXMax $ getArea b'
           vhb <- fillVHintBoard =<< (addVHint (newEmptyVHintBoard (0,y) (getXMax $ getArea b')) =<< evalState (genHint s') (us,g''))
           hhb <- fillHHintBoard $ newEmptyHHintBoard (x,0) (getYMax $ getArea b')
           --print $ evalState (genHint s') (us,g'')
           --print =<<
           --print us
           --print b'
           --print b
           --print s'
           --print a
           return Game
               { board    = b'
               , solution = s'
               , gen      = g
               , vhb      = vhb
               , hhb      = hhb
               }

    --gameInit :: Int -> IO (IORef Game)
    --gameInit seed = do
    --    let g = mkStdGen seed

    --    nC <- read <$> getSetting "columns"
    --    nR <- read <$> getSetting "rows"

    --    is <- read <$> getSetting "initialSolved"

    --    b  <- newBoard nR nC (0,0)

    --    let (s, (_, g'))    = runState (genSolution nR nC) ([1..nC],g)
    --    do s' <- s
    --       print s'
    --       let (b',(_,g'')) = runState (initialSol is b s') (concat [[(r,c)
    --                                                        | r <- [0..nR-1]]
    --                                                        | c <- [0..nC-1]]
    --                                                        ,g')
    --       let y = getYMax $ getArea b'
    --           x = getXMax $ getArea b'
    --       vhb <- fillVHintBoard $ newEmptyVHintBoard (0,y) (getXMax $ getArea b')
    --       hhb <- fillHHintBoard $ newEmptyHHintBoard (x,0) (getYMax $ getArea b')
    --       --print a
    --       newIORef Game
    --           { board    = b'
    --           , solution = s'
    --           , gen      = g
    --           , vhb      = vhb
    --           , hhb      = hhb
    --           }
