module Game.Board.Row
(
    Row,
    squares,

    newRow,
    genSolvedRow,

    getRowSquare,
    setRowSquare,
) where

    import Control.Monad
    import Control.Monad.Trans.State

    import Data.List
    import Data.Maybe

    import System.Random

    import Game.Board.Square
    import Game.SolutionState

    import UI.Coordinate
    import UI.Input
    import UI.Input.Settings
    import UI.Render


    data Row = Row { rowNum  :: Int
                   , squares :: [Square]
                   }

    instance Eq Row where
        r == r' = rowNum r == rowNum r'

    instance Ord Row where
        r <= r' = rowNum r <= rowNum r'

    instance Renderable Row where
        render  w = mapM_ (render w) . squares
        getArea   = foldl (\/) Empty . map getArea . squares

    instance Clickable Row where
        lclick ui pt r = let ss = squares r
                             rN = rowNum  r
                          in do ss' <- mapM (lclick ui pt) ss
                                if ss == ss'
                                 then return $ Row rN   ss'
                                 else return $ Row rN $ map (removeVal
                                                        (val $ head (ss'\\ss)))
                                                        ss'

        rclick ui pt r = do ss <- mapM (rclick ui pt) $ squares r
                            let sols = map fromJust   $ filter isJust
                                                      $ map getSolution ss
                                ss' = map (\v -> foldl (flip removeVal) v
                                          sols) ss
                            return Row { rowNum = rowNum r, squares = ss' }

    instance Solvable Row where
        (Row _ []    )|-|(Row _  []      ) = Correct
        (Row r (s:ss))|-|(Row r' (s':ss')) = (s|-|s')-|-(Row r ss|-|Row r' ss')


    newRow :: Int -> Int -> Point -> IO Row
    newRow r nC (x,y) = fmap (Row r) $ sequence $ squareIter nC $ return x
        where
            squareIter :: Int -> IO Coord -> [IO Square]
            squareIter 0 _ = []
            squareIter c x =
                let s   = do x'  <- x
                             unsolvedSquare [1..nC] r nC (x',y)
                    x'' = do bw' <- bw
                             s'  <- s
                             return $ getXMax (getArea s') + bw'
                 in s : squareIter (c-1) x''

            bw :: IO Coord
            bw = read <$> getSetting "tileSpacing"

    genSolvedRow :: Int -> Int -> State ([Int], StdGen) (IO Row)
    genSolvedRow nC r = fmap (Row r) . sequence
                                    <$> replicateM nC (genSolvedSquare nC r)



    getRowSquare :: Row -> Int -> Square
    getRowSquare r 0 = head $ squares r
    getRowSquare r c = getRowSquare (Row (rowNum r) (tail $ squares r)) (c-1)

    setRowSquare :: Row -> Int -> Square -> Row
    setRowSquare r c s'
        | null (squares r) = r
        | otherwise        = Row rN $ s'' : ss'
        where s''    = if c == 0
                           then transplantSolution s s'
                           else removeVal (val s') s
              (s:ss) = squares r
              ss'    = squares $ setRowSquare (Row rN ss) (c-1) s'
              rN     = rowNum r
