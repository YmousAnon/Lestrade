module Game.Board.Row
(
    Row,
    newRow,
    genSolvedRow,

    colMap,

    --getNewRow,
) where

    import Control.Monad
    import Control.Monad.Trans.State

    import Data.List

    import System.Random

    import Game.Board.Square

    data Row = Row
                { squares :: [Square]
                }

    instance Show Row where
        show Row { squares = ss } = show ss

    newRow :: Int -> Int -> Row
    newRow r nC = Row $ map (square [0..nC-1] r) [0..nC-1]

    genSolvedRow :: Int -> Int -> State ([Int],StdGen) Row
    genSolvedRow r nC = fmap Row $ replicateM nC $ genSolvedSquare r nC



    colMap :: (Int -> Square -> Square) -> Row -> Row
    colMap f (Row ps) = Row $ map (\p -> f (col p) p) ps


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
