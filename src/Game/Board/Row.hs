module Game.Board.Row
(
    Row,

    newRow,
    genSolvedRow,

    getRowSquare,
    setRowSquare,
) where

    import Control.Monad
    import Control.Monad.Trans.State

    import Data.List

    import System.Random

    import Game.Board.Square

    import Interface.Coordinate
    import Interface.Input
    import Interface.Render

    import Settings


    data Row = Row { squares :: [Square] }

    instance Show Row where
        show = show . squares

    instance Renderable Row where
        render  w = mapM_ (render w) . squares
        getArea   = foldl (\/) Empty . map getArea . squares

    instance Clickable Row where
        lclick pt r = let ss = squares r
                       in do ss' <- mapM (lclick pt) ss
                             if ss == ss'
                              then return $ Row   ss'
                              else return $ Row $ map (removeVal
                                                      (val $ head (ss'\\ss)))
                                                      ss'

        rclick pt r = do ss <- mapM (rclick pt) $ squares r
                         return Row { squares = ss }



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


    genSolvedRow :: Int -> State ([Int], StdGen) (IO Row)
    genSolvedRow nC = fmap Row . sequence
                             <$> replicateM nC (genSolvedSquare nC)



    getRowSquare :: Row -> Int -> Square
    getRowSquare (Row (s:ss)) 0 = s
    getRowSquare (Row (s:ss)) c = getRowSquare (Row (ss)) (c-1)

    setRowSquare :: Row -> Int -> Square -> Row
    setRowSquare (Row [])     c s' = Row []
    setRowSquare (Row (s:ss)) c s' = Row $ s'' : squares (setRowSquare (Row ss)
                                                                       (c-1)
                                                                       s')
        where s'' = if c == 0
                        then transplantSolution s s'
                        else removeVal (val s') s
