{-# LANGUAGE ViewPatterns #-}
module Game.Hints.Horizontal
(
    HHint,
    newHHint,

    HHintBoard,
    newEmptyHHintBoard,
    fillHHintBoard,
) where

    import Control.Monad.Trans.State

    import Game.Board.Value

    import Interface.Coordinate
    import Interface.Input.Settings
    import Interface.Render
    import Interface.Render.Primitive


    data HHint = HHint
                 { vals     :: [Value]
                 , area     :: Area
                 , bgrgb    :: [Double]
                 , selected :: Bool
                 , hidden   :: Bool
                 }

    instance Renderable HHint where
        render w HHint
            { vals  = vs
            , area  = a
            , bgrgb = rgb
            } = renderColour w a rgb
             >> mapM_ (render w) vs
        getArea  = area

    newHHint :: [Value] -> Point -> IO HHint
    newHHint vs (x,y) = do
        tw  <- read <$> getSetting "tileWidth"
        hbw <- read <$> getSetting "hintBorderWidth"

        bgt <- value 0 True (x,y) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        let vs' = valList 3 (x+hbw,y+hbw) vs bgt
            avs = newArea (x,y) (2*hbw+3*tw) (2*hbw+tw)
        return HHint
            { vals     = vs'
            , area     = avs
            , bgrgb    = bgc
            , selected = False
            , hidden   = False
            }
        where
            valList :: Int -> Point -> [Value] -> Value -> [Value]
            valList 0 _     _  _   = []
            valList i (x,y) vs bgv = v' : valList (i-1)
                                                  (getXMax $ getArea v',y)
                                                  vs'
                                                  bgv
                 where
                    v'  | null vs   = moveValueTo bgv       (x,y)
                        | otherwise = moveValueTo (head vs) (x,y)
                    vs' | null vs   = []
                        | otherwise = tail vs



    data HHintBoard = HHintBoard
                      { hints  :: [HHint]
                      , xy     :: Point
                      , height :: Coord
                      }

    instance Renderable HHintBoard where
        render w    = mapM_ (render w) . hints
        getArea hhb = foldl (\/) (newArea (xy hhb) 0 0)
                    $ map getArea (hints hhb)

    newEmptyHHintBoard :: Point -> Coord -> HHintBoard
    newEmptyHHintBoard xy h = HHintBoard
                              { hints  = []
                              , xy     = xy
                              , height = h
                              }

    addHHint :: HHintBoard -> HHint -> HHintBoard
    addHHint HHintBoard { hints = hs     , xy = xy, height = h' } h =
             HHintBoard { hints = hs++[h], xy = xy, height = h' }

    fillHHintBoard :: HHintBoard -> IO HHintBoard
    fillHHintBoard vhb  = do
        h <- nextPos vhb >>= newHHint []

        if getXMax (getArea h) <= getXMax (getArea vhb) || null (hints vhb)
            then fillHHintBoard $ addHHint vhb h
            else return vhb

    nextPos :: HHintBoard -> IO Point
    nextPos HHintBoard { hints = [], xy = xy } = return xy
    nextPos HHintBoard
        { hints  = (reverse -> (h:_))
        , xy     = (x,y)
        , height = h'
        } = xy'' . read <$> getSetting "hintSpacing"
        where
            (x',y')  = (getXMin $ getArea h,getYMax $ getArea h)
            xy'' hs' = if h' < y' + getHeight (getArea h)
                           then (hs' + getXMax (getArea h),y)
                           else (x',hs'+y')
