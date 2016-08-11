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


    newVHint :: HintType -> [Value] -> Point -> IO Hint
    newVHint ht vs (x,y) = do
        tw  <- read <$> getSetting "tileWidth"
        hbw <- read <$> getSetting "hintBorderWidth"

        bgv <- value 0 True (x,y) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        let vs' = valList 3 (x+hbw,y+hbw) vs bgv
            a   = newArea (x,y) (2*hbw+tw) (2*hbw+3*tw)
            as  = map getArea vs'

        newHint vs' a as Vertical ht
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
    genVHint (ri,ci) s = do
        ioHT <- genHintType Vertical

        ri'  <- getRowI $ delete ri              [0..length (rows s)]
        ri'' <- getRowI $ delete ri' $ delete ri [0..length (rows s)]

        let rs = [ri,ri',ri'']

        return $ ioHT >>= \ht ->
            print (ri,ci) >>
            case ht of
                VTwo   -> genVTwoHint   (sort $ take 2 rs) ci s
                VThree -> genVThreeHint (sort rs)          ci s
        where
            getRowI :: [Int] -> State StdGen Int
            getRowI rs = (rs !!) <$> state (randomR (0,length rs-2))

    genVTwoHint :: [Int] -> Int -> Board -> IO Hint
    genVTwoHint rs ci s = newVHint VTwo (map getV rs) (0,0)
        where
            getV :: Int -> Value
            getV ri = val $ getRowSquare (rows s !! ri) ci

    genVThreeHint :: [Int] -> Int -> Board -> IO Hint
    genVThreeHint rs ci s = newVHint VThree (map getV rs) (0,0)
        where
            getV :: Int -> Value
            getV i = val $ getRowSquare (rows s !! i) ci



    nextVHintPos :: Hint -> Point -> Coord -> IO Point
    nextVHintPos h (x,y) w = xy'' . read <$> getSetting "hintSpacing"
        where
            (x',y')  = (getXMax $ getArea h,getYMin $ getArea h)
            xy'' hs' = if w < x' + getWidth (getArea h)
                           then (x ,hs' + getYMax (getArea h))
                           else (hs' + x',y')
