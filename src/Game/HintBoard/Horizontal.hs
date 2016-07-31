module Game.HintBoard.Horizontal
(
    newHHint,
    genHHint,

    nextHHintPos,
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


    newHHint :: [Value] -> Point -> IO Hint
    newHHint vs (x,y) = do
        tw  <- read <$> getSetting "tileWidth"
        hbw <- read <$> getSetting "hintBorderWidth"

        bgt <- value 0 True (x,y) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        let vs' = valList 3 (x+hbw,y+hbw) vs bgt
            avs = newArea (x,y) (2*hbw+3*tw) (2*hbw+tw)
        return Hint
               { vals     = vs'
               , len      = length vs
               , area     = avs
               , bgrgb    = bgc
               , selected = False
               , hidden   = False
               , hOrient  = Horizontal
               }
        where valList :: Int -> Point -> [Value] -> Value -> [Value]
              valList 0 _     _  _   = []
              valList i (x,y) vs bgv = v' : valList (i-1)
                                                    (getXMax $ getArea v',y)
                                                    vs'
                                                    bgv
                   where v'  | null vs   = moveValueTo bgv       (x,y)
                             | otherwise = moveValueTo (head vs) (x,y)
                         vs' | null vs   = []
                             | otherwise = tail vs

    genHHint :: (Int,Int) -> Board -> State StdGen (IO Hint)
    genHHint (ri,ci) s = do
        ioHT <- genHintType Horizontal

        let ri' = 2
        ci'  <- getColI
        ci'' <- getColI
        let rci  = (ri ,ci )
            rci' = (ri',ci')

        return $ ioHT >>= \ht ->
                 case ht of
                     HStandard -> genStandardHHint [rci',rci,rci'] s
        where
            getColI :: State StdGen Int
            getColI = state $ randomR (0,length (rows s)-1)

    genStandardHHint :: [(Int,Int)] -> Board -> IO Hint
    genStandardHHint rs s = newHHint (map (uncurry getV) rs) (0,0)
        where
            getV :: Int -> Int -> Value
            getV ri ci = val $ getRowSquare (rows s !! ri) ci



    nextHHintPos :: Hint -> Point -> Coord -> IO Point
    nextHHintPos h (x,y) w = xy'' . read <$> getSetting "hintSpacing"
        where
            (x',y')  = (getXMin $ getArea h,getYMax $ getArea h)
            xy'' hs' = if w < y' + getHeight (getArea h)
                           then (hs' + getXMax (getArea h), y)
                           else (x',hs' + y')
