module Game.HintBoard
(
    HintBoard,
    newEmptyHintBoard,

    fillHintBoard,
    addHint,
    genHint,
) where

    import Control.Monad.Trans.State

    import Game.Board
    import Game.Board.Value
    import Game.HintBoard.Hint
    import Game.HintBoard.Vertical
    import Game.HintBoard.Horizontal

    import Interface.Coordinate
    import Interface.Input
    import Interface.Input.Settings
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
        rclick pt hb = do hs'  <- mapM (rclick pt) (hints hb)
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

         in if nulltest || (vtest) || ((not vertical) && htest)
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
                          Vertical   -> newVHint vs xy
                          Horizontal -> newHHint vs xy

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



    genHint :: (Int,Int) -> Board -> State StdGen (IO Hint)
    genHint rci s = do
        o <- genOrientation

        case o of
            Vertical   -> genVHint rci s
            Horizontal -> genHHint rci s
        where
            genOrientation :: State StdGen (Orientation)
            genOrientation = return Vertical
            --genOrientation = state $ randomR (Vertical, Horizontal)

    ----genHint (ri,ci) s = do nR <- getHintN

    ----                       rs <- let rs' = deleteI ri $ rows s
    ----                              in shuffle rs' <$> order (length rs')
    ----                       let r  = rows s !! ri

    ----                           vs = map (\r' -> val $ getRowSquare r' ci) $
    ----                                    sort (r:take (nR-1) rs)
    ----                       return $ newHint vs (0,0)
    ----    where
    ----        getHintN :: State StdGen (Int)
    ----        getHintN = state $ randomR (2, 3)

    ----        order :: Int -> State StdGen [Int]
    ----        order 0 = return []
    ----        order n = do i  <- state $ randomR (0,n-1)
    ----                     is <- order (n-1)
    ----                     return (i:is)

    ----        shuffle :: [a] -> [Int] -> [a]
    ----        shuffle xs []     = []
    ----        shuffle xs (i:is) = xs !! i : shuffle (deleteI i xs) is

    ----        deleteI :: Int -> [a] -> [a]
    ----        deleteI i xs = take i xs++drop (i+1) xs
