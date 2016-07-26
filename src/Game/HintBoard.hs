--{-# LANGUAGE ViewPatterns #-}
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

    import Interface.Coordinate
    import Interface.Input
    import Interface.Input.Settings
    import Interface.Render

    import System.Random


--    genHint :: Board -> State ([(Int,Int)],StdGen) (IO VHint)
--    genHint s = do (rcs,g ) <- get
--                   let (rci,g' ) = randomR (0,length rcs-1) g
--                       (h  ,g'') = runState (genVHint (rcs !! rci) s) g'
--                   put (rcs,g'')
--                   return $ h
--
--        where
--            order :: Int -> State StdGen [Int]
--            order n = do i  <- state $ randomR (0,n-1)
--                         is <- order (n-1)
--                         return (i:is)
--
--







    data HintBoard = HintBoard
                     { hints  :: [Hint]
                     , xy     :: Point
                     , width  :: Coord
                     , hbtype :: HintType
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
                         else mapM unSelectHint hs'
            return HintBoard
                   { hints  = hs''
                   , xy     = xy     hb
                   , width  = width  hb
                   , hbtype = hbtype hb
                   }
        rclick pt hb = do hs' <- mapM (rclick pt) (hints hb)
                          return HintBoard
                                 { hints  = hs'
                                 , xy     = xy    hb
                                 , width  = width hb
                                 , hbtype = hbtype hb
                                 }

    newEmptyHintBoard :: Point -> Coord -> HintType -> HintBoard
    newEmptyHintBoard xy w ht = HintBoard
                                { hints  = []
                                , xy     = xy
                                , width  = w
                                , hbtype = ht
                                }



    fillHintBoard :: HintBoard -> IO HintBoard
    fillHintBoard hb  = nextPos hb >>= newHint (hbtype hb) [] >>= \h ->

        if getYMax (getArea h) <= getYMax (getArea hb) || null (hints hb)
            then fillHintBoard =<< addHint h hb
            else return hb



    addHint :: Hint -> HintBoard -> IO HintBoard
    addHint h hb = do
        pt <- nextPos hb
        return HintBoard
               { hints  = hints  hb++[moveTo pt h]
               , xy     = xy     hb
               , width  = width  hb
               , hbtype = hbtype hb
               }

    newHint :: HintType -> [Value] -> Point -> IO Hint
    newHint ht vs xy = case ht of
                          Vertical -> newVHint ht vs xy

    nextPos :: HintBoard -> IO Point
    nextPos HintBoard { hints = [], xy = xy } = return xy
    nextPos HintBoard
        { hints  = hs
        , xy     = xy'
        , width  = w
        , hbtype = ht
        }
        | ht == Vertical = nextVHintPos (last hs) xy' w

--xy'' . read <$> getSetting "hintSpacing"
--where
--    (x',y')  = (getXMax $ getArea h,getYMin $ getArea h)
--    xy'' hs' = if w < x' + getWidth (getArea h)
--                   then (x ,hs' + getYMax (getArea h))
--                   else (hs' + x',y')







    genHint :: (Int,Int) -> Board -> State StdGen (IO Hint)
    genHint rci s = do
        ht <- genHintType

        case ht of
            Vertical   -> genVHint ht rci s
            --Horizontal -> genHHint ht rci s
        where
            genHintType :: State StdGen (HintType)
            genHintType = return Vertical
            --genHintType = state $ randomR (Vertical, Horizontal)

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
