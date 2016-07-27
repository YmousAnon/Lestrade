module Game.HintBoard.Vertical
(
    newVHint,
    genVHint,

    nextVHintPos,
) where

    import Control.Monad.Trans.State

    import Data.List

    import Game.Board
    import Game.Board.Row
    import Game.Board.Square
    import Game.Board.Value
    import Game.HintBoard.Hint

    import Interface.Coordinate
    import Interface.Input.Settings
    import Interface.Render

    import System.Random


    newVHint :: [Value] -> Point -> IO Hint
    newVHint vs (x,y) = do
        tw  <- read <$> getSetting "tileWidth"
        hbw <- read <$> getSetting "hintBorderWidth"

        bgt <- value 0 True (x,y) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        let vs' = valList 3 (x+hbw,y+hbw) vs bgt
            avs = newArea (x,y) (2*hbw+tw) (2*hbw+3*tw)
        return Hint
               { vals     = vs'
               , len      = length vs
               , area     = avs
               , bgrgb    = bgc
               , selected = False
               , hidden   = False
               , hOrient  = Vertical
               }
        where valList :: Int -> Point -> [Value] -> Value -> [Value]
              valList 0 _     _  _   = []
              valList i (x,y) vs bgv = v' : valList (i-1)
                                                    (x,getYMax $ getArea v')
                                                    vs'
                                                    bgv
                   where v'  | null vs   = moveValueTo bgv       (x,y)
                             | otherwise = moveValueTo (head vs) (x,y)
                         vs' | null vs   = []
                             | otherwise = tail vs

    genVHint :: (Int,Int) -> Board -> State StdGen (IO Hint)
    genVHint (ri,ci) s = do nR <- getVHintN
                            rs <- let rs' = deleteI ri $ rows s
                                   in shuffle rs' <$> order (length rs')
                            let r  = rows s !! ri
                                vs = map (\r' -> val $ getRowSquare r' ci) $
                                         sort (r:take (nR-1) rs)

                            return $ newVHint vs (0,0)
        where
            getVHintN :: State StdGen (Int)
            getVHintN = state $ randomR (2, 3)

            order :: Int -> State StdGen [Int]
            order 0 = return []
            order n = do i  <- state $ randomR (0,n-1)
                         is <- order (n-1)
                         return (i:is)

            shuffle :: [a] -> [Int] -> [a]
            shuffle xs []     = []
            shuffle xs (i:is) = xs !! i : shuffle (deleteI i xs) is

            deleteI :: Int -> [a] -> [a]
            deleteI i xs = take i xs++drop (i+1) xs



    nextVHintPos :: Hint -> Point -> Coord -> IO Point
    nextVHintPos h (x,y) w = xy'' . read <$> getSetting "hintSpacing"
        where
            (x',y')  = (getXMax $ getArea h,getYMin $ getArea h)
            xy'' hs' = if w < x' + getWidth (getArea h)
                           then (x ,hs' + getYMax (getArea h))
                           else (hs' + x',y')
