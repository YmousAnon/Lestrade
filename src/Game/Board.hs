module Game.Board
(
    Board,

    newBoard,
    solvedBoard,
) where

    import Control.Monad.Trans.State

    import Interface.Render

    import Game.Board.Row
    --import Game.Board.Square
    --import Game.Board.Value

    import Data.List

    import System.Random

    data Board = Board
                { rows :: [Row]
                }

    instance Show Board where
        show Board { rows = [] } = ""
        show Board { rows = rs } = concatMap (\r -> '\n':show r) rs

    instance Renderable Board where
        render Board { rows = rs } = mapM_ render rs
    board :: [Row] -> Board
    board rs = Board
        { rows = rs
        }
    --data Row   = Row          [Piece]
    --    deriving Show

    --data Piece = Solution     Int
    --           | Alternatives [[Int]]
    --    deriving Show

    --instance Show Solution where

    --genSolution :: Int -> Int -> StdGen -> Board
    --genSolution nR nC g = Board $ map (\r -> genSolvedRow r nC g) [0..nR-1]
    solvedBoard :: Int -> Int -> Float -> (Float,Float) -> State StdGen Board
    solvedBoard nR nC w xy = board <$> mapM (rowIter nC w xy) [0..nR-1]
        where
            rowIter :: Int -> Float -> (Float,Float) -> Int -> State StdGen Row
            rowIter nC w (x,y) ri = state $ \g ->
                let xy' = (x,y+w*1.1*w)
                    (r, ([],g')) = runState (genSolvedRow ri nC w xy')
                                            ([0..nC-1],g)
                in  (r,g')

    newBoard :: Int -> Int -> Float -> (Float,Float) -> Board
    newBoard nR nC w (x,y) = Board $ map rowIter [1..nR]
        where
            rowIter :: Int -> Row
            rowIter ri = newRow ri nC w (x,y+w*1.1*w)
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
