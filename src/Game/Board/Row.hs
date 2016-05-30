module Game.Board.Row
(
    Row,
    newRow,

    colMap,

    genSolvedRow,
) where

    import Game.Board.Square

    import Data.List

    import System.Random


    data Row = Row
                { squares :: [Square]
                }
        deriving(Show)


    newRow :: Int -> Int -> Row
    newRow r nC = Row $ map (newSquare [0..nC-1] r) [0..nC-1]


    colMap :: (Int -> Square -> Square) -> Row -> Row
    colMap f (Row ps) = Row $ map (\p -> f (col p) p) ps


    --genSolvedRow :: Int -> Int -> StdGen -> ([Square],StdGen)
    --genSolvedRow r [] g = ([]                             ,g  )
    --genSolvedRow r c g = (newSquare [cs!!i] r [cs!!i]:cs',g'')
    --genSolvedRow = 12
    genSolvedRow :: Int -> Int -> StdGen -> (Row,StdGen)
    genSolvedRow r nC g =
            let (vs,g') = scrambleCols [0..nC-1] g
            in  (Row $ map (\(v,c) -> newSquare [v] r c) $ zip vs [0..nC], g')
        where

            scrambleCols :: [Int] -> StdGen -> ([Int], StdGen)
            scrambleCols [] g = ([],g)
            scrambleCols cs g = (cs!!i:cs', g')
                where
                    (i  ,g' ) = randomR      (0,length cs-1)     g
                    (cs',g'') = scrambleCols (delete (cs!!i) cs) g'

