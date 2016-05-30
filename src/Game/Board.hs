module Game.Board
(
    Board,

    genSolution,
) where

    import Control.Monad.Trans.State
    import Control.Monad

    import Data.List

    import System.Random


    import Game.Board.Row
    import Game.Board.Square
    import Game.Board.Value

    data Board = Board        [Row]
        deriving Show

    --data Row   = Row          [Piece]
    --    deriving Show

    --data Piece = Solution     Int
    --           | Alternatives [[Int]]
    --    deriving Show

    --instance Show Solution where

    --genSolution :: Int -> Int -> StdGen -> Board
    --genSolution nR nC g = Board $ map (\r -> genSolvedRow r nC g) [0..nR-1]
    genSolution :: Int -> Int -> State StdGen [Row]
    genSolution nR nC = mapM (\r -> state $ genSolvedRow r nC) [0..nC-1]
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
