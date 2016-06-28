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
        render  window = mapM_ (render window) . squares
        getArea        = foldl (\/) Empty . map getArea . squares
        --getArea r = foldl (\/) Empty $ map getArea $ squares r

    newRow :: Int -> Int -> Point -> IO Row
    newRow r nC (x,y) = fmap Row $ sequence $ squareIter nC $ return x
        where
            squareIter :: Int -> IO Coord -> [IO Square]
            squareIter 0 _ = []
            squareIter c x =
                let s   = x  >>= \x'  ->
                          bw >>= \bw' -> unsolvedSquare [1..nC] r nC (x'+bw',y)
                    x'' = (getXMax . getArea) <$> s
                 in s : squareIter (c-1) x''

            bw :: IO Coord
            bw = read <$> getSetting "tileBorderWidth"


    genSolvedRow :: Int -> Int -> Point -> IO (State ([Int], StdGen) (IO Row))
    genSolvedRow r nC (x,y) = do--TODO: Fix this to be more like newRow
        tw <- read <$> getSetting "tileWidth"
        bw <- read <$> getSetting "tileBorderWidth"

        let xys = (\c -> (x+bw+(bw+tw)*2*fromIntegral c,y)) <$> [0..nC-1]
         in return $ (fmap Row . sequence <$> mapM (genSolvedSquare r nC) xys)

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
