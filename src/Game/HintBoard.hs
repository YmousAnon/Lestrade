module Game.HintBoard
(
    HintBoard,
    newEmptyHintBoard,

    fillHintBoard,
    addHint,
) where

    import Control.Monad.Trans.State

    import Game.Board
    import Game.Board.Value
    import Game.HintBoard.Hint
    import Game.HintBoard.Vertical
    import Game.HintBoard.Horizontal

    import Interface.Coordinate
    import Interface.Input
    import Interface.Render

    import System.Random


    data HintBoard = HintBoard
                     { hints       :: [Hint]
                     , xy          :: Point
                     , width       :: Coord
                     , orientation :: Orientation
                     }

    instance Renderable HintBoard where
        render w   = mapM_ (render w) . hints
        getArea hb = foldl (\/) (newArea (xy hb) 0 0)
                    $ map getArea (hints hb)

    instance Clickable HintBoard where
        lclick pt hb = do
            hs'  <- swapSelectedHint =<< mapM (lclick pt) (hints hb)
            hs'' <- if any (inArea pt . getArea) hs'
                         then return             hs'
                         else mapM unSelectHint  hs'
            return HintBoard
                   { hints       = hs''
                   , xy          = xy     hb
                   , width       = width  hb
                   , orientation = orientation hb
                   }
        rclick pt hb = do
            hs'  <- mapM (rclick pt) (hints hb)
            hs'' <- mapM unSelectHint  hs'
            return HintBoard
                   { hints       = hs''
                   , xy          = xy    hb
                   , width       = width hb
                   , orientation = orientation hb
                   }

    newEmptyHintBoard :: Point -> Coord -> Orientation -> HintBoard
    newEmptyHintBoard xy w o = HintBoard
                               { hints       = []
                               , xy          = xy
                               , width       = w
                               , orientation = o
                               }



    fillHintBoard :: HintBoard -> IO HintBoard
    fillHintBoard hb = nextHintPos hb >>= newHint (orientation hb) [] >>= \h ->
        let vertical = orientation hb == Vertical
            nulltest = null (hints hb)
            htest    = getXMax (getArea h) == getXMax (getArea hb)
            vtest    = getYMax (getArea h) == getYMax (getArea hb)

         in if nulltest || vtest || (not vertical && htest)
                then addHint h hb >>= fillHintBoard
                else return hb

    addHint :: Hint -> HintBoard -> IO HintBoard
    addHint h hb = do
        pt <- nextHintPos hb
        return HintBoard
               { hints       = hints  hb++[moveTo pt h]
               , xy          = xy     hb
               , width       = width  hb
               , orientation = orientation hb
               }

    newHint :: Orientation -> [Value] -> Point -> IO Hint
    newHint o vs xy = case o of
                          Vertical   -> newVHint VHEmpty vs xy
                          Horizontal -> newHHint VHEmpty vs xy

    nextHintPos :: HintBoard -> IO Point
    nextHintPos HintBoard { hints = [], xy = xy } = return xy
    nextHintPos HintBoard
        { hints       = hs
        , xy          = xy'
        , width       = w
        , orientation = o
        }
        | o == Vertical   = nextVHintPos (last hs) xy' w
        | o == Horizontal = nextHHintPos (last hs) xy' w
