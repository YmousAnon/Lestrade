module Game.Board.Row
(
    Row,
    newRow,
    genSolvedRow,

    --colMap,

    --getNewRow,
) where

    import Control.Monad
    import Control.Monad.Trans.State

    --import Graphics.Rendering.OpenGL

    import Data.List

    import System.Random

    import Game.Board.Square

    import Interface.Coordinate
    import Interface.Render

    import Settings

    data Row = Row
                { squares :: [Square]
                }

    instance Show Row where
        show Row { squares = ss } = show ss

    instance Renderable Row where
        --getTexture Row { squares = ss } = concatMapM getTexture ss
        render Row { squares = ss } = mapM_ render ss

    newRow :: Int -> Int -> Point -> IO Row
    newRow r nC (x,y) = do
        tw <- read <$> getSetting "tileWidth"
        bw <- read <$> getSetting "tileBorderWidth"
        print bw

        let xys = map (\c -> (x+bw+(bw+tw)*2*fromIntegral c,bw+y)) [0..nC-1]
         in Row <$> mapM (unsolvedSquare [1..nC] r nC) xys

            --xys = map (\c -> (x + tw*2.2*(fromIntegral c),y)) [1..nC]
    --newRow r nC w (x,y) = Row $ map (uncurry (square [0..nC-1] r)) [0..nC-1]

    genSolvedRow :: Int -> Int -> Point -> IO (State ([Int], StdGen) (IO Row))
    genSolvedRow r nC (x,y) = do
        tw <- read <$> getSetting "tileWidth"
        bw <- read <$> getSetting "tileBorderWidth"

        let xys = (\c -> (x+bw+(bw+tw)*2*fromIntegral c,y)) <$> [0..nC-1]
         in return $ fmap Row <$> sequence <$> mapM (genSolvedSquare r nC) xys

    --genSolvedRow :: Int -> Int -> Point -> State ([Int],StdGen) (IO Row)
    --genSolvedRow r nC (x,y) = fmap Row <$> sequence
    --                                   <$> mapM (genSolvedSquare r nC) xys
    --    where
    --        xys = map (\c -> (x+bw+(bw+tw)*2*fromIntegral c,y)) [0..nC-1]




    --colMap :: (Int -> Square -> Square) -> Row -> Row
    --colMap f (Row ps) = Row $ map (\p -> f (col p) p) ps


    --getNewRow :: Int -> Int -> Row
    --getNewRow r nC = Row $ map (getNewSquare r nC) [0..nC-1]

    --genSolvedRow :: Int -> Int -> State StdGen Row
    --genSolvedRow r nC g =
    --        let (vs,g') = scrambleCols [0..nC-1] g
    --        in  (Row $ map (\(v,c) -> newSquare [v] r c) $ zip vs [0..nC], g')
    --    where

    --        scrambleCols :: [Int] -> StdGen -> ([Int], StdGen)
    --        scrambleCols [] g = ([],g)
    --        scrambleCols cs g = (cs!!i:cs', g')
    --            where
    --                (i  ,g' ) = randomR      (0,length cs-1)     g
    --                (cs',g'') = scrambleCols (delete (cs!!i) cs) g'
    --genSolvedRow :: Int -> Int -> StdGen -> (Row,StdGen)
    --genSolvedRow r nC g =
    --        let (vs,g') = scrambleCols [0..nC-1] g
    --        in  (Row $ map (\(v,c) -> newSquare [v] r c) $ zip vs [0..nC], g')
    --    where

    --        scrambleCols :: [Int] -> StdGen -> ([Int], StdGen)
    --        scrambleCols [] g = ([],g)
    --        scrambleCols cs g = (cs!!i:cs', g')
    --            where
    --                (i  ,g' ) = randomR      (0,length cs-1)     g
    --                (cs',g'') = scrambleCols (delete (cs!!i) cs) g'
