{-# LANGUAGE ViewPatterns #-}
module Game.Hints.Vertical
(
    VHint,
    newVHint,

    VHintBoard,
    newEmptyVHintBoard,
    fillVHintBoard,
) where

    import Control.Monad.Trans.State

    import Game.Board.Value

    import Interface.Coordinate
    import Interface.Render
    import Interface.Render.Primitive

    import Interface.Input.Settings

    data VHint = VHint
                 { vals     :: [Value]
                 , area     :: Area
                 , bgrgb    :: [Double]
                 , selected :: Bool
                 , hidden   :: Bool
                 }

    instance Renderable VHint where
        render w VHint
            { vals  = vs
            , area  = a
            , bgrgb = rgb
            } = renderColour w a rgb
             >> mapM_ (render w) vs
        getArea = area

    newVHint :: [Value] -> Point -> IO VHint
    newVHint vs (x,y) = do
        tw  <- read <$> getSetting "tileWidth"
        hbw <- read <$> getSetting "hintBorderWidth"

        bgt <- value 0 True (x,y) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        let vs' = valList 3 (x+hbw,y+hbw) vs bgt
            avs = newArea (x,y) (2*hbw+tw) (2*hbw+3*tw)
        return VHint
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
                                                  (x,getYMax $ getArea v')
                                                  vs'
                                                  bgv
                 where
                    v'  | null vs   = moveValueTo bgv       (x,y)
                        | otherwise = moveValueTo (head vs) (x,y)
                    vs' | null vs   = []
                        | otherwise = tail vs



    data VHintBoard = VHintBoard
                      { hints :: [VHint]
                      , xy    :: Point
                      , width :: Coord
                      }

    instance Renderable VHintBoard where
        render w    = mapM_ (render w) . hints
        getArea vhb = foldl (\/) (newArea (xy vhb) 0 0)
                    $ map getArea (hints vhb)

    newEmptyVHintBoard :: Point -> Coord -> VHintBoard
    newEmptyVHintBoard xy w = VHintBoard
                              { hints = []
                              , xy    = xy
                              , width = w
                              }

    addVHint :: VHintBoard -> VHint -> VHintBoard
    addVHint VHintBoard { hints = hs     , xy = xy, width = w } h =
             VHintBoard { hints = hs++[h], xy = xy, width = w }

    fillVHintBoard :: VHintBoard -> IO VHintBoard
    fillVHintBoard vhb  = do
        h <- nextPos vhb >>= newVHint []

        if getYMax (getArea h) <= getYMax (getArea vhb) || null (hints vhb)
            then fillVHintBoard $ addVHint vhb h
            else return vhb

    nextPos :: VHintBoard -> IO Point
    nextPos VHintBoard { hints = [], xy = xy } = return xy
    nextPos VHintBoard
        { hints = (reverse -> (h:_))
        , xy    = (x,y)
        , width = w
        } = xy'' . read <$> getSetting "hintSpacing"
        where
            (x',y')  = (getXMax $ getArea h,getYMin $ getArea h)
            xy'' hs' = if w < x' + getWidth (getArea h)
                           then (x ,hs' + getYMax (getArea h))
                           else (hs' + x',y')
