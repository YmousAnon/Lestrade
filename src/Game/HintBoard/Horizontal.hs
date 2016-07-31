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


    newHHint :: HintType -> [Value] -> Point -> IO Hint
    newHHint ht vs (x,y) = do
        tw  <- read <$> getSetting "tileWidth"
        hbw <- read <$> getSetting "hintBorderWidth"

        bgt <- value 0 True (x,y) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        let vs' = valList 3 (x+hbw,y+hbw) vs bgt
            avs = newArea (x,y) (2*hbw+3*tw) (2*hbw+tw)
        return Hint
               { vals     = vs'
               , area     = avs
               , bgrgb    = bgc
               , selected = False
               , hidden   = False
               , hOrient  = Horizontal
               , hType    = ht
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

        ri'   <- getRowI [ri]     (length (rows s))
        ri''  <- getRowI [ri,ri'] (length (rows s))

        ci'  <- getColI
        ci'' <- getColI

        let rci   = (ri  ,ci  )
            rci'  = (ri' ,ci' )
            rci'' = (ri'',ci'')

        rev  <- state random

        return $ ioHT >>= \ht ->
            case ht of
                HNeighbour -> genHNeighbourHint [rci',rci,rci']      s
                HSpear     -> genHSpearHint     [rci,rci',rci''] rev s
        where
            getRowI :: [Int] -> Int -> State StdGen Int
            getRowI ris lrs
                | maximum ris == lrs-1 = return (minimum ris-1)
                | minimum ris == 0     = return (maximum ris+1)
                | otherwise            = ([minimum ris-1,maximum ris+1] !!) <$>
                                         state (randomR (0,1))

            getColI :: State StdGen Int
            getColI = state $ randomR (0,length (rows s)-1)

    genHNeighbourHint :: [(Int,Int)] -> Board -> IO Hint
    genHNeighbourHint rs s = newHHint HNeighbour (map (uncurry getV) rs) (0,0)
        where
            getV :: Int -> Int -> Value
            getV ri ci = val $ getRowSquare (rows s !! ri) ci

    genHSpearHint :: [(Int,Int)] -> Bool -> Board -> IO Hint
    genHSpearHint rcis rev s = newHHint HSpear (map (uncurry getV) rcis') (0,0)
        where
            rcis' :: [(Int,Int)]
            rcis' = (if rev then reverse else id) $ sortBy fstGT rcis

            fstGT :: (Int,a) -> (Int,a) -> Ordering
            fstGT (r,_) (r',_) = if r < r' then GT else LT

            getV :: Int -> Int -> Value
            getV ri ci = val $ getRowSquare (rows s !! ri) ci



    nextHHintPos :: Hint -> Point -> Coord -> IO Point
    nextHHintPos h (x,y) w = xy'' . read <$> getSetting "hintSpacing"
        where
            (x',y')  = (getXMin $ getArea h,getYMax $ getArea h)
            xy'' hs' = if w < y' + getHeight (getArea h)
                           then (hs' + getXMax (getArea h), y)
                           else (x',hs' + y')
