module Game.Board
(
    Board,

    newBoard,
    --solvedBoard,
) where

    import Control.Monad.Trans.State

    import Interface.Coordinate
    import Interface.Render

    import Game.Board.Row
    --import Game.Board.Square
    --import Game.Board.Value

    import Data.List

    import Settings

    import System.Random

    data Board = Board
                { rows :: [Row]
                }

    instance Show Board where
        show Board { rows = [] } = ""
        show Board { rows = rs } = concatMap (\r -> '\n':show r) rs

    instance Renderable Board where
        render  window = mapM_ (render window) . rows
        getArea        = foldl (\/) Empty . map getArea . rows

        --getArea b = foldl (\/) Empty $ map getArea $ squares r

    --board :: [Row] -> Board
    --board rs = Board
    --    { rows = rs
    --    }
    --data Row   = Row          [Piece]
    --    deriving Show

    --data Piece = Solution     Int
    --           | Alternatives [[Int]]
    --    deriving Show

    --instance Show Solution where

    --genSolution :: Int -> Int -> StdGen -> Board
    --genSolution nR nC g = Board $ map (\r -> genSolvedRow r nC g) [0..nR-1]
    --solvedBoard :: Int -> Int -> Float -> (Float,Float) -> State StdGen Board
    --solvedBoard nR nC w xy = board <$> mapM (rowIter nC w xy) [0..nR-1]
    --    where
    --        rowIter :: Int -> Float -> (Float,Float) -> Int -> State StdGen Row
    --        rowIter nC w (x,y) ri = state $ \g ->
    --            let xy' = (x,y+w*rowDiffY nC ri)
    --                (r, ([],g')) = runState (genSolvedRow ri nC w xy')
    --                                        ([0..nC-1],g)
    --            in  (r,g')

    newBoard :: Int -> Int -> Point -> IO Board
    newBoard nR nC (x,y) = fmap Board $ sequence $ rowIter nR $ return y
        where
            rowIter :: Int -> IO Coord -> [IO Row]
            rowIter 0  _ = []
            rowIter ri y =
                let r   = y  >>= \y'  ->
                          bw >>= \bw' -> newRow ri nC (x,y'+bw')
                    y'' = getYMax . getArea <$> r
                 in r : rowIter (ri-1) y''

            bw :: IO Coord
            bw = read <$> getSetting "rowBorderWidth"


    --rowDiffY :: Int -> Int -> Float
    --rowDiffY nC ri
    --    | mod nC 2 == 0 = 1.1*(fromIntegral $ div ((ri-1)*nC) 2 :: Float)
    --    | otherwise     = rowDiffY (nC+1) ri
        --where
        --    genSolvedRow' :: Int -> Int -> State StdGen Row
        --    genSolvedRow' nC i = state $ \g ->
        --        let (r, ([],g')) = runState (genSolvedRow i nC) ([0..nC-1],g)
        --        in  (r,g')
    --mapM state $ (\r -> genSolvedRow r nC g) [0..nR-1]
    --genSolution = 12


    --rollNDice :: Int -> State StdGen [Int]
    --rollNDice n = replicateM n rollDie
    --    where rollDie = state $ randomR (1, 6)
    --    --getSolution :: StdGen -> (Board,StdGen)
    --getSolution g = (\(b,g) -> (Board b,g)) $ genSolvedRows 6 6 g
    --    where
    --        genSolvedRows :: Int -> Int -> StdGen -> ([Row],StdGen)
    --        genSolvedRows 0  _  g = ([],g)
    --        genSolvedRows nR nC g = (Row r:rs,g'')
    --            where
    --                (r ,g' ) = genSolvedRow [0..nC]    g
    --                (rs,g'') = genSolvedRows (nR-1) nC g'




    --getEmptyBoard :: Board
    --getEmptyBoard =
