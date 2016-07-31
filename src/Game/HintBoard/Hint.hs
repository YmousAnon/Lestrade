module Game.HintBoard.Hint
(
    Orientation (Vertical,Horizontal),

    HintType (VHEmpty,HNeighbour,HSpear,VThree,VTwo),
    genHintType,

    Hint (Hint,vals,area,bgrgb,selected,hidden,hOrient,hType),

    toggleSelectHint,
    selectHint,
    unSelectHint,
    swapSelectedHint,
    swapTwo,

    toggleHideHint,
) where

    import Control.Monad.Trans.State

    import Game.Board.Value

    import Interface.Coordinate
    import Interface.Input
    import Interface.Input.Settings
    import Interface.Render
    import Interface.Render.Primitive

    import System.Random


    data Orientation = Vertical | Horizontal
        deriving (Enum,Eq)

    instance Random Orientation where
        random g        = case randomR (0,1) g of
                              (r, g') -> (toEnum r, g')
        randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                              (r, g') -> (toEnum r, g')



    data HintType = VHEmpty
                  | HNeighbour | HSpear
                  | VThree | VTwo

    genHintType :: Orientation -> State StdGen (IO HintType)
    genHintType o = (if o == Vertical then genVHintType else genHHintType)
                          <$> state random
        where
            genHHintType :: Int -> IO HintType
            genHHintType i = do
                wHNeighbour <- read <$> getSetting "wHNeighbour"
                wHSpear     <- read <$> getSetting "wHSpear"

                return $ (replicate wHNeighbour HNeighbour++
                          replicate wHSpear     HSpear
                         ) !! mod i (wHNeighbour+wHSpear)

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
                }

    instance Show Hint where
        show Hint { vals = vs } = concatMap (\v -> '\n':show v) vs++"\n"

    instance Renderable Hint where
        render w Hint
            { vals  = vs
            , area  = a
            , bgrgb = rgb
            } = renderColour w a rgb
             >> mapM_ (render w) vs
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
                       }

    instance Clickable Hint where
        lclick pt h = if inArea pt (area h)
                          then toggleSelectHint $ unHideHint h
                          else return                        h
        rclick pt h = if inArea pt (area h)
                          then return $ toggleHideHint       h
                          else return                        h



    toggleSelectHint :: Hint -> IO Hint
    toggleSelectHint vh = if selected vh then unSelectHint vh
                                          else selectHint   vh

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
                  in Hint
            { vals     = map (changeCol [0.25,0.25,0.25]) vsh++vst
            , area     = area     h
            , bgrgb    = bgrgb    h
            , selected = selected h
            , hidden   = True
            , hOrient  = hOrient  h
            , hType    = hType   h
            }

    unHideHint :: Hint -> Hint
    unHideHint h = let (vsh,vst) = splitValList (hType h) (vals h)
                    in Hint
            { vals     = map (changeCol [1,1,1]) vsh++vst
            , area     = area     h
            , bgrgb    = bgrgb    h
            , selected = selected h
            , hidden   = False
            , hOrient  = hOrient  h
            , hType    = hType   h
            }

    splitValList :: HintType -> [Value] -> ([Value],[Value])
    splitValList ht = splitAt (ind ht)
        where
            ind :: HintType -> Int
            ind VHEmpty = 0
            ind VTwo    = 2
            ind _       = 3
