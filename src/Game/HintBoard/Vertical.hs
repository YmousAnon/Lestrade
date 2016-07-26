--{-# LANGUAGE ViewPatterns #-}
module Game.HintBoard.Vertical
(
    newVHint,
    genVHint,

    nextVHintPos,
) where

    --import Control.Monad

    --import Game.Board.Square

    --import Interface.Coordinate
    --import Interface.Input
    --import Interface.Input.Settings
    --import Interface.Render
    --import Interface.Render.Primitive

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
               , htype    = ht
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


    genVHint :: HintType -> (Int,Int) -> Board -> State StdGen (IO Hint)
    genVHint ht (ri,ci) s = do nR <- getVHintN
                               rs <- let rs' = deleteI ri $ rows s
                                      in shuffle rs' <$> order (length rs')
                               let r  = rows s !! ri
                                   vs = map (\r' -> val $ getRowSquare r' ci) $
                                            sort (r:take (nR-1) rs)

                               return $ newVHint ht vs (0,0)
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




--    data VHint = VHint
--                 { vals     :: [Value]
--                 , len      :: Int
--                 , area     :: Area
--                 , bgrgb    :: [Float]
--                 , selected :: Bool
--                 , hidden   :: Bool
--                 }
--
--    instance Show VHint where
--        --show VHint { vals = vs } = "Testastic"
--        show VHint { vals = vs } = concatMap (\v -> '\n':show v) vs++"\n"
--
--    instance Renderable VHint where
--        render w VHint
--            { vals  = vs
--            , area  = a
--            , bgrgb = rgb
--            } = renderColour w a rgb
--             >> mapM_ (render w) vs
--        getArea = area
--
--    instance Movable VHint where
--        moveTo xy  vh = moveBy (xy>-<(getAreaStart $ area vh)) vh
--        moveBy dxy vh = VHint
--                        { vals     = map (moveBy dxy) $ vals vh
--                        , len      = len                     vh
--                        , area     = moveBy dxy       $ area vh
--                        , bgrgb    = bgrgb                   vh
--                        , selected = selected                vh
--                        , hidden   = hidden                  vh
--                        }
--
--    instance Clickable VHint where
--        lclick pt vh = if inArea pt (area vh) then toggleSelectVHint        vh
--                                              else return                   vh
--        rclick pt vh = if inArea pt (area vh) then return $ toggleHideVHint vh
--                                              else return                   vh
--
--
--
--    newVHint :: [Value] -> Point -> IO VHint
--    newVHint vs (x,y) = do
--        tw  <- read <$> getSetting "tileWidth"
--        hbw <- read <$> getSetting "hintBorderWidth"
--
--        bgt <- value 0 True (x,y) 0
--        bgc <- map (/255) . read <$> getSetting "tilergb"
--
--        let vs' = valList 3 (x+hbw,y+hbw) vs bgt
--            avs = newArea (x,y) (2*hbw+tw) (2*hbw+3*tw)
--        return VHint
--            { vals     = vs'
--            , len      = length vs
--            , area     = avs
--            , bgrgb    = bgc
--            , selected = False
--            , hidden   = False
--            }
--        where
--            valList :: Int -> Point -> [Value] -> Value -> [Value]
--            valList 0 _     _  _   = []
--            valList i (x,y) vs bgv = v' : valList (i-1)
--                                                  (x,getYMax $ getArea v')
--                                                  vs'
--                                                  bgv
--                 where
--                    v'  | null vs   = moveValueTo bgv       (x,y)
--                        | otherwise = moveValueTo (head vs) (x,y)
--                    vs' | null vs   = []
--                        | otherwise = tail vs
--
--    genVHint :: (Int,Int) -> Board -> State StdGen (IO VHint)
--    genVHint (ri,ci) s = do nR <- getVHintN
--
--                            rs <- let rs' = deleteI ri $ rows s
--                                   in shuffle rs' <$> order (length rs')
--                                   --in shuffle rs' <$> order (length rs'-1)
--                            let r  = rows s !! ri
--
--                                vs = map (\r' -> val $ getRowSquare r' ci) $
--                                         sort (r:take (nR-1) rs)
--                            --map (\r' -> getBoardSquare s r' c) (r:rs)
--                            --return (length rs)
--                            --return (take ri (rows s)++drop (ri+1) (rows s))
--
--
--
--                            --rs' = shuffle' rs (length rs)
--                            --rs = runState (randomR (0, length (rows s)))
--                            --rs = replicateM nR $ runState $
--                            --let i = 4
--                            --    j = 3
--                            --sort . take j . shuffle [0..i] <$>(order i)--newVHint vs (0,0)
--                            return $ newVHint vs (0,0)--shuffle rs <$> order i
--        where
--            getVHintN :: State StdGen (Int)
--            getVHintN = state $ randomR (2, 3)
--
--            order :: Int -> State StdGen [Int]
--            order 0 = return []
--            order n = do i  <- state $ randomR (0,n-1)
--                         is <- order (n-1)
--                         return (i:is)
--
--            shuffle :: [a] -> [Int] -> [a]
--            shuffle xs []     = []
--            shuffle xs (i:is) = xs !! i : shuffle (deleteI i xs) is
--
--            deleteI :: Int -> [a] -> [a]
--            deleteI i xs = take i xs++drop (i+1) xs
--
--
--
--
--    toggleSelectVHint :: VHint -> IO VHint
--    toggleSelectVHint vh = if selected vh then unSelectVHint vh
--                                          else selectVHint   vh
--
--    selectVHint :: VHint -> IO VHint
--    selectVHint vh = do
--        bgrgb' <- map (/255) . read <$> getSetting "hintselectedrgb"
--        return VHint { vals     = vals     vh
--                     , len      = len      vh
--                     , area     = area     vh
--                     , bgrgb    = bgrgb'
--                     , selected = True
--                     , hidden   = hidden   vh
--                     }
--
--    unSelectVHint :: VHint -> IO VHint
--    unSelectVHint vh = do
--        bgrgb' <- map (/255) . read <$> getSetting "hintrgb"
--        return VHint { vals     = vals     vh
--                     , len      = len      vh
--                     , area     = area     vh
--                     , bgrgb    = bgrgb'
--                     , selected = False
--                     , hidden   = hidden   vh
--                     }
--
--
--
--    toggleHideVHint :: VHint -> VHint
--    toggleHideVHint vh = if hidden vh then unHideVHint vh else hideVHint vh
--
--    hideVHint :: VHint -> VHint
--    hideVHint vh = let vsh = take (len vh) (vals vh)
--                       vst = drop (len vh) (vals vh)
--                    in VHint
--            { vals     = map (changeCol [0.25,0.25,0.25]) vsh++vst
--            , len      = len      vh
--            , area     = area     vh
--            , bgrgb    = bgrgb    vh
--            , selected = selected vh
--            , hidden   = True
--            }
--
--    unHideVHint :: VHint -> VHint
--    unHideVHint vh = let vsh = take (len vh) (vals vh)
--                         vst = drop (len vh) (vals vh)
--                      in VHint
--            { vals     = map (changeCol [1,1,1]) vsh++vst
--            , len      = len      vh
--            , area     = area     vh
--            , bgrgb    = bgrgb    vh
--            , selected = selected vh
--            , hidden   = False
--            }
--
--
--
--    data VHintBoard = VHintBoard
--                      { hints :: [VHint]
--                      , xy    :: Point
--                      , width :: Coord
--                      }
--
--    instance Renderable VHintBoard where
--        render w    = mapM_ (render w) . hints
--        getArea vhb = foldl (\/) (newArea (xy vhb) 0 0)
--                    $ map getArea (hints vhb)
--
--    instance Clickable VHintBoard where
--        lclick pt vhb = do
--            vhs'  <- swapSelected =<< mapM (lclick pt) (hints vhb)
--            vhs'' <- if any (inArea pt . getArea) vhs'
--                         then return             vhs'
--                         else mapM unSelectVHint vhs'
--            return VHintBoard
--                   { hints = vhs''
--                   , xy    = xy    vhb
--                   , width = width vhb
--                   }
--        rclick pt vhb = do hs' <- mapM (rclick pt) (hints vhb)
--                           return VHintBoard
--                                  { hints = hs'
--                                  , xy    = xy    vhb
--                                  , width = width vhb
--                                  }
--
--
--
--    newEmptyVHintBoard :: Point -> Coord -> VHintBoard
--    newEmptyVHintBoard xy w = VHintBoard
--                              { hints = []
--                              , xy    = xy
--                              , width = w
--                              }
--
--    addVHint :: VHintBoard -> VHint -> IO VHintBoard
--    addVHint vhb h = do
--        pt <- nextPos vhb
--        return VHintBoard { hints = hs++[moveTo pt h], xy = xy', width = w }
--        --return VHintBoard { hints = hs++[moveTo pt h], xy = xy', width = w }
--        where
--            hs  = hints vhb
--            xy' = xy    vhb
--            w   = width vhb
--
--    fillVHintBoard :: VHintBoard -> IO VHintBoard
--    fillVHintBoard vhb  = do
--        h <- nextPos vhb >>= newVHint []
--
--        if getYMax (getArea h) <= getYMax (getArea vhb) || null (hints vhb)
--            then fillVHintBoard =<< addVHint vhb h
--            else return vhb
--
--    nextPos :: VHintBoard -> IO Point
--    nextPos VHintBoard { hints = [], xy = xy } = return xy
--    nextPos VHintBoard
--        { hints = (reverse -> (h:_))
--        , xy    = (x,y)
--        , width = w
--        } = xy'' . read <$> getSetting "hintSpacing"
--        where
--            (x',y')  = (getXMax $ getArea h,getYMin $ getArea h)
--            xy'' hs' = if w < x' + getWidth (getArea h)
--                           then (x ,hs' + getYMax (getArea h))
--                           else (hs' + x',y')
--
--
--
--
--    swapSelected :: [VHint] -> IO[VHint]
--    swapSelected vhs = let sel   = filter selected         vhs
--                           nosel = filter (not . selected) vhs
--                        in if length sel == 2
--                               then (++nosel) <$> swapTwo sel
--                               else return vhs
--
--    swapTwo :: [VHint] -> IO[VHint]
--    swapTwo [vh0,vh1] =
--        let vh0' = unSelectVHint $ moveTo (getAreaStart $ getArea vh1) vh0
--            vh1' = unSelectVHint $ moveTo (getAreaStart $ getArea vh0) vh1
--         in sequence [vh0',vh1']