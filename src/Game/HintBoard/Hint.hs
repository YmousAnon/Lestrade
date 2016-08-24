module Game.HintBoard.Hint
(
    Orientation (Vertical,Horizontal),
    genHintOrientation,

    HintType (VHEmpty,VThree,VTwo,HNeighbour,HSpear,HInverseSpear),
    genHintType,

    Hint(hOrient),
    newHint,

    toggleSelectHint,
    selectHint,
    unSelectHint,
    swapSelectedHint,
    swapTwo,

    toggleHideHint,
) where

    import Control.Monad.Trans.State

    import Data.Set (fromList)

    import Game.Board.Value
    import Game.HintBoard.Decoration

    import Interface.Coordinate
    import Interface.Input
    import Interface.Input.Settings
    import Interface.Render
    import Interface.Render.Primitive

    import System.Random


    data Orientation = Vertical | Horizontal
        deriving (Eq)

    genHintOrientation :: State StdGen (IO Orientation)
    genHintOrientation = getHintOrientation <$> state random
        where
            getHintOrientation :: Int -> IO Orientation
            getHintOrientation i = do
                wVertical   <- read <$> getSetting "wVertical"
                wHorizontal <- read <$> getSetting "wHorizontal"

                return $ (replicate wVertical   Vertical++
                          replicate wHorizontal Horizontal
                         ) !! mod i (wVertical+wHorizontal)




    data HintType = VHEmpty
                  | VThree | VTwo
                  | HSpear | HInverseSpear | HNeighbour

        deriving(Eq,Ord)

    genHintType :: Orientation -> State StdGen (IO HintType)
    genHintType o = (if o == Vertical then genVHintType else genHHintType)
                          <$> state random
        where
            genHHintType :: Int -> IO HintType
            genHHintType i = do
                wHNeighbour    <- read <$> getSetting "wHNeighbour"
                wHSpear        <- read <$> getSetting "wHSpear"
                wHInverseSpear <- read <$> getSetting "wHInverseSpear"

                return $ (replicate wHNeighbour    HNeighbour++
                          replicate wHSpear        HSpear    ++
                          replicate wHInverseSpear HInverseSpear
                         ) !! mod i (wHNeighbour+wHSpear+wHInverseSpear)

            genVHintType :: Int -> IO HintType
            genVHintType i = do
                wVThree    <- read <$> getSetting "wVThree"
                wVTwo      <- read <$> getSetting "wVTwo"

                return $ (replicate wVThree VThree++
                          replicate wVTwo   VTwo
                         ) !! mod i (wVThree+wVTwo)



    data Hint = Hint
                { vals     :: [Value]
                , area     :: Area
                , bgrgb    :: [Float]
                , selected :: Bool
                , hidden   :: Bool
                , hOrient  :: Orientation
                , hType    :: HintType
                , decs     :: [Decoration]
                }

    instance Eq Hint where
        h == h' = let vs  = rmDups $ vals' h
                      vs' = rmDups $ vals' h'
                      overlap = filter (`elem` vs) vs'
                   in length overlap >= 2
            where
                rmDups  = foldl (\acc x -> if x `elem` acc then acc
                                                           else acc++[x]) []
                vals' x = if hType x == HInverseSpear
                              then [head(vals x),last(vals x)]
                              else vals x

    instance Ord Hint where
        h `compare` h' = hType h `compare` hType h'

    instance Show Hint where
        show Hint { vals = vs } = concatMap (\v -> '\n':show v) vs++"\n"

    instance Renderable Hint where
        render w Hint
            { vals  = vs
            , area  = a
            , bgrgb = rgb
            , decs  = ds
            } = renderColour w a rgb
             >> mapM_ (render w) vs
             >> mapM_ (render w) ds
        getArea = area

    instance Movable Hint where
        moveTo xy  h = moveBy (xy>-<getAreaStart (area h)) h
        moveBy dxy h = Hint
                       { vals     = map (moveBy dxy) $ vals h
                       , area     = moveBy dxy       $ area h
                       , bgrgb    = bgrgb                   h
                       , selected = selected                h
                       , hidden   = hidden                  h
                       , hOrient  = hOrient                 h
                       , hType    = hType                   h
                       , decs     = map (moveBy dxy) $ decs h
                       }

    instance Clickable Hint where
        lclick pt h = if inArea pt (area h)
                          then toggleSelectHint $ unHideHint h
                          else return                        h
        rclick pt h = if inArea pt (area h)
                          then return $ toggleHideHint       h
                          else return                        h

    newHint :: [Value] -> Area -> [Area] -> Orientation -> HintType -> IO Hint
    newHint vs a as o ht = do
        bgc   <- map (/255) . read <$> getSetting "tilergb"
        decs' <- getDecorationList as ht

        return Hint
               { vals     = vs
               , area     = a
               , bgrgb    = bgc
               , selected = False
               , hidden   = False
               , hOrient  = o
               , hType    = ht
               , decs     = decs'
               }



    getDecorationList :: [Area] -> HintType -> IO [Decoration]
    getDecorationList _          VHEmpty       = return []
    getDecorationList _          VThree        = return []
    getDecorationList _          VTwo          = return []
    getDecorationList _          HNeighbour    = return []
    getDecorationList [a,a',a''] HSpear        = sequence
        [newDecoration (a\/a'\/a'') Spear]
    getDecorationList [a,a',a''] HInverseSpear = sequence
        [newDecoration a' Inversion,newDecoration (a\/a'\/a'') Spear]



    toggleSelectHint :: Hint -> IO Hint
    toggleSelectHint vh = (if selected vh then unSelectHint else selectHint) vh

    selectHint :: Hint -> IO Hint
    selectHint h = do
        bgrgb' <- map (/255) . read <$> getSetting "hintselectedrgb"
        return Hint { vals     = vals    h
                    , area     = area    h
                    , bgrgb    = bgrgb'
                    , selected = True
                    , hidden   = hidden  h
                    , hOrient  = hOrient h
                    , hType    = hType   h
                    , decs     = decs    h
                    }

    unSelectHint :: Hint -> IO Hint
    unSelectHint h = do
        bgrgb' <- map (/255) . read <$> getSetting "hintrgb"
        return Hint { vals     = vals    h
                    , area     = area    h
                    , bgrgb    = bgrgb'
                    , selected = False
                    , hidden   = hidden  h
                    , hOrient  = hOrient h
                    , hType    = hType   h
                    , decs     = decs    h
                    }



    swapSelectedHint :: [Hint] -> IO[Hint]
    swapSelectedHint vhs = let sel   = filter selected         vhs
                               nosel = filter (not . selected) vhs
                            in if length sel == 2
                                   then (++nosel) <$> swapTwo sel
                                   else return vhs

    swapTwo :: [Hint] -> IO[Hint]
    swapTwo [vh0,vh1] =
        let vh0' = unSelectHint $ moveTo (getAreaStart $ getArea vh1) vh0
            vh1' = unSelectHint $ moveTo (getAreaStart $ getArea vh0) vh1
         in sequence [vh0',vh1']



    toggleHideHint :: Hint -> Hint
    toggleHideHint h = if hidden h then unHideHint h else hideHint h

    hideHint :: Hint -> Hint
    hideHint h = let (vsh,vst) = splitValList (hType h) (vals h)
                     shade     = map (0.15*) [1,1,1]
                  in Hint
            { vals     = map (changeValueColour shade) vsh++vst
            , area     = area     h
            , bgrgb    = bgrgb    h
            , selected = selected h
            , hidden   = True
            , hOrient  = hOrient  h
            , hType    = hType    h
            , decs     = map (changeDecorationColour shade) (decs h)
            }

    unHideHint :: Hint -> Hint
    unHideHint h = let (vsh,vst) = splitValList (hType h) (vals h)
                    in Hint
            { vals     = map (changeValueColour [1,1,1]) vsh++vst
            , area     = area     h
            , bgrgb    = bgrgb    h
            , selected = selected h
            , hidden   = False
            , hOrient  = hOrient  h
            , hType    = hType    h
            , decs     = map (changeDecorationColour [1,1,1]) (decs h)
            }

    splitValList :: HintType -> [Value] -> ([Value],[Value])
    splitValList ht = splitAt (ind ht)
        where
            ind :: HintType -> Int
            ind VHEmpty = 0
            ind VTwo    = 2
            ind _       = 3
