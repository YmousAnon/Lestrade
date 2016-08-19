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

        bgv <- value 0 True (x,y) 0

        let vs' = valList 3 (x+hbw,y+hbw) vs bgv
            a   = newArea (x,y) (2*hbw+3*tw) (2*hbw+tw)
            as  = map getArea vs'

        newHint vs' a as Horizontal ht
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

        ri'  <- getRowI
        ri'' <- getRowI

        ci'  <- getColI [ci]     (length $ squares $ head $ rows s)
        ci'' <- getColI [ci,ci'] (length $ squares $ head $ rows s)

        let rcis  = [(ri,ci),(ri',ci'),(ri'',ci'')]

        rev    <- state random
        invSel <- state random

        return $ ioHT >>= \ht ->
            print (ri,ci) >>
            case ht of
                HNeighbour    -> genHNeighbourHint    rcis            s
                HSpear        -> genHSpearHint        rcis rev        s
                HInverseSpear -> genHInverseSpearHint rcis rev invSel s
        where
            getRowI :: State StdGen Int
            getRowI = state $ randomR (0,length (rows s)-1)

            getColI :: [Int] -> Int -> State StdGen Int
            getColI cis lcs
                | maximum cis == lcs-1 = return (minimum cis-1)
                | minimum cis == 0     = return (maximum cis+1)
                | otherwise            = ([minimum cis-1,maximum cis+1] !!) <$>
                                         state (randomR (0,1))

    genHNeighbourHint :: [(Int,Int)] -> Board -> IO Hint
    genHNeighbourHint rcis s =
        newHHint HNeighbour (map (uncurry getV) rcis') (0,0)
        where
            rcis' :: [(Int,Int)]
            rcis' = let [rci,rci',_] = rcis in [rci,rci',rci]

            getV :: Int -> Int -> Value
            getV ri ci = val $ getRowSquare (rows s !! ri) ci

    genHSpearHint :: [(Int,Int)] -> Bool -> Board -> IO Hint
    genHSpearHint rcis rev s = newHHint HSpear (map (uncurry getV) rcis') (0,0)
        where
            rcis' :: [(Int,Int)]
            rcis' = (if rev then reverse else id) $ sortBy fstGT rcis

            getV :: Int -> Int -> Value
            getV ri ci = val $ getRowSquare (rows s !! ri) ci

    genHInverseSpearHint :: [(Int,Int)] -> Bool -> Int -> Board -> IO Hint
    genHInverseSpearHint rcis rev invSel s = do
        nC <- read <$> getSetting "columns"
        let rcis'   = (if rev then reverse else id) $ sortBy fstGT rcis
            ci'     = delete (snd(rcis'!!1)) [0..nC-1] !! mod invSel (nC-1)
            rcis''  = [head rcis',(fst (rcis'!!1),ci'),rcis'!!2]

        newHHint HInverseSpear (map (uncurry getV) rcis'') (0,0)
        where
            getV :: Int -> Int -> Value
            getV ri ci = val $ getRowSquare (rows s !! ri) ci

    fstGT :: (a,Int) -> (a,Int) -> Ordering
    fstGT (_,c) (_,c') = compare c c'



    nextHHintPos :: Hint -> Point -> Coord -> IO Point
    nextHHintPos h (x,y) w = xy'' . read <$> getSetting "hintSpacing"
        where
            (x',y')  = (getXMin $ getArea h,getYMax $ getArea h)
            xy'' hs' = if w < y' + getHeight (getArea h)
                           then (hs' + getXMax (getArea h), y)
                           else (x',hs' + y')
