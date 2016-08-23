module Game.SolutionState.Celebration
(
    Celebration,

    newCelebration,
    updateCelebration,
) where

    import Control.Monad.Trans.State

    import Data.Either
    import Data.Maybe

    import Game.Board
    import Game.Board.Square
    import Game.Board.Value

    import Graphics.UI.GLFW

    import Interface.Coordinate
    import Interface.Input.Settings
    import Interface.Render

    import System.Random


    data Celebration = Grid
                       {  gen      :: StdGen
                       , tiles    :: [[Either Point Value]]
                       , gridNC   :: Int
                       , gridNR   :: Int
                       , board    :: Board
                       , boardNC  :: Int
                       , boardNR  :: Int
                       }

    instance Eq Celebration where
        Grid{} == Grid{} = True
        _      == _      = False

    instance Renderable Celebration where
        render w Grid { tiles = ts } = mapM_ (render w) $ rights $ concat ts
        getArea  c                   = Empty

    newCelebration :: Board -> Area -> StdGen ->  IO Celebration
    newCelebration s a g = do
        nR  <- read <$> getSetting "rows"
        nC  <- read <$> getSetting "columns"

        tW  <- read <$> getSetting "tileWidth"

        return $ newGrid s nR nC a tW g

    updateCelebration :: Celebration -> IO Celebration
    updateCelebration c
        | c == Grid {} = updateGrid c


    addValue :: Either Point Value -> Value -> Either Point Value
    addValue (Left pt) v' = Right $ moveTo pt                         v'
    addValue (Right v) v' = Right $ moveTo (getAreaStart $ getArea v) v'



    newGrid :: Board -> Int -> Int -> Area -> Coord -> StdGen ->
               Celebration
    newGrid s nR nC a tW g = let ts = tilesOuter (0,0)
                              in Grid
                                 {  gen      = g
                                 , tiles    = ts
                                 , gridNC   = length (head ts)
                                 , gridNR   = length ts
                                 , board    = s
                                 , boardNC  = nC
                                 , boardNR  = nR
                                 }
        where
            tilesOuter :: Point -> [[Either Point Value]]
            tilesOuter (x,y)
                | getYMax a < y = []
                | otherwise     = tilesInner (x,y) : tilesOuter (x,y+tW)

            tilesInner :: Point -> [Either Point Value]
            tilesInner (x,y)
                | getXMax a < x = []
                | otherwise     = Left (x,y)       : tilesInner (x+tW,y)

    updateGrid :: Celebration -> IO Celebration
    updateGrid c = return Grid
                          { gen      = g'
                          , tiles    = ts'
                          , gridNC   = gNC
                          , gridNR   = gNR
                          , board    = s
                          , boardNC  = bNC
                          , boardNR  = bNR
                          }
        where
            ts  = tiles   c
            bNC = boardNC c
            bNR = boardNR c

            s   =  board  c
            gNC = gridNC  c
            gNR = gridNR  c

            ts' = setGridOuter (tiles c) gr gc
                where
                    v = fromJust $ getSolution $ getBoardSquare s br bc

                    setGridOuter :: [[Either Point Value]] -> Int -> Int ->
                                    [[Either Point Value]]
                    setGridOuter (t:ts) 0 c = setGridInner t c : ts
                    setGridOuter (t:ts) r c = t : setGridOuter ts (r-1) c

                    setGridInner :: [Either Point Value] -> Int ->
                                    [Either Point Value]
                    setGridInner (t:ts) 0 = addValue t v : ts
                    setGridInner (t:ts) c = t : setGridInner ts (c-1)


            ([br,bc,gr,gc],g') = runState genPositions (gen c)
                where
                    genPositions ::State StdGen [Int]
                    genPositions = mapM genPosition [bNR,bNC,gNR,gNC]

                    genPosition :: Int -> State StdGen Int
                    genPosition n = state $ randomR (0,n-1)

