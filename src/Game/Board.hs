module Game.Board
(
    Board,

    newBoard,
    genSolution,
) where

    import Control.Monad
    import Control.Monad.Trans.State

    import Interface.Coordinate
    import Interface.Input
    import Interface.Render

    import Game.Board.Row

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

    instance Clickable Board where
        lclick pt b = mapM (lclick pt) (rows b) >>= \rs ->
            return Board { rows = rs
                         }
        rclick pt b = mapM (rclick pt) (rows b) >>= \rs ->
            return Board { rows = rs
                         }



    genSolution :: Int -> Int -> State ([Int], StdGen) (IO Board)
    genSolution nR nC = fmap Board . sequence <$> replicateM nR
                                                             (genSolvedRow nC)

    newBoard :: Int -> Int -> Point -> IO Board
    newBoard nR nC (x,y) = fmap Board $ sequence $ rowIter nR $ return y
        where
            rowIter :: Int -> IO Coord -> [IO Row]
            rowIter 0  _ = []
            rowIter ri y =
                let r    = do y'  <- y
                              bw' <- bw
                              newRow ri nC (x,y'+bw')
                    y''  = getYMax . getArea <$> r
                 in r : rowIter (ri-1) y''

            bw :: IO Coord
            bw = read <$> getSetting "rowBorderWidth"
