module Game.Board
(
    Board,

    newBoard,
    genSolution,

    initialSol,
    initialSol',
) where

    import Control.Monad
    import Control.Monad.Trans.State

    import Interface.Coordinate
    import Interface.Input
    import Interface.Render

    import Game.Board.Row
    import Game.Board.Square
    import Game.Board.Value

    import Data.List

    import Settings

    import System.Random


    data Board = Board { rows :: [Row] }

    instance Show Board where
        show Board { rows = rs } | null rs   = ""
                                 | otherwise = concatMap (\r -> '\n':show r) rs

    instance Renderable Board where
        render  window = mapM_ (render window) . rows
        getArea        = foldl (\/) Empty . map getArea . rows

    instance Clickable Board where
        lclick pt b = fmap Board (mapM (lclick pt) (rows b))
        rclick pt b = fmap Board (mapM (rclick pt) (rows b))



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

    genSolution :: Int -> Int -> State ([Int], StdGen) (IO Board)
    genSolution nR nC = fmap Board . sequence <$> replicateM nR
                                                             (genSolvedRow nC)



    getBoardSquare :: Board -> Int -> Int -> Square
    getBoardSquare (Board (r':rs)) 0 c = getRowSquare   r'               c
    getBoardSquare (Board (r':rs)) r c = getBoardSquare (Board rs) (r-1) c

    setBoardSquare :: Board -> Int -> Int -> Square -> Board
    setBoardSquare (Board (r':rs)) 0 c s = Board $
        setRowSquare r' c s : rs
    setBoardSquare (Board (r':rs)) r c s = Board $
        r'                  : rows (setBoardSquare (Board rs) (r-1) c s)

    swapSquare :: Board -> Board -> Int -> Int -> Board
    swapSquare bFrom bTo r c = setBoardSquare bFrom r c
                             $ getBoardSquare bTo   r c

    getPos :: State ([(Int,Int)], StdGen) (Int,Int)
    getPos = do (is,g) <- get
                let (n,g') = randomR (0,length is-1) g
                    i      = is !! n
                    is'    = delete i is
                put (is',g')
                return i

    initialSol :: Int -> Board -> Board -> State ([(Int,Int)], StdGen) Board
    initialSol n b s = foldr (\i' b' -> uncurry (swapSquare b' s) i') b
                   <$> replicateM n getPos

    initialSol' n = replicateM n getPos
