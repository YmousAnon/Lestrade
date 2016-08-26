module Game.HintBoard
(
    HintBoard(hbOrient),
    newEmptyHintBoard,

    fillHintBoard,
    addHintList,

    scrambleIndices,
    genHintList,
    removeDuplicates,
    removeContained,
) where

    import Control.Monad.Trans.State

    import Data.List

    import Game.Board
    import Game.Board.Value
    import Game.HintBoard.Hint
    import Game.HintBoard.Vertical
    import Game.HintBoard.Horizontal

    import UI.Coordinate
    import UI.Input
    import UI.Render

    import System.Random


    data HintBoard = HintBoard
                     { hints    :: [Hint]
                     , xy       :: Point
                     , width    :: Coord
                     , hbOrient :: Orientation
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
                   { hints    = hs''
                   , xy       = xy       hb
                   , width    = width    hb
                   , hbOrient = hbOrient hb
                   }
        rclick pt hb = do
            hs'  <- mapM (rclick pt) (hints hb)
            hs'' <- mapM unSelectHint  hs'
            return HintBoard
                   { hints    = hs''
                   , xy       = xy    hb
                   , width    = width hb
                   , hbOrient = hbOrient hb
                   }

    newEmptyHintBoard :: Point -> Coord -> Orientation -> HintBoard
    newEmptyHintBoard xy w o = HintBoard
                               { hints    = []
                               , xy       = xy
                               , width    = w
                               , hbOrient = o
                               }



    fillHintBoard :: HintBoard -> IO HintBoard
    fillHintBoard hb = nextHintPos hb >>= newEmptyHint (hbOrient hb) [] >>=
        \h ->
        let vertical = hbOrient hb == Vertical
            nulltest = null (hints hb)
            htest    = getXMax (getArea h) == getXMax (getArea hb)
            vtest    = getYMax (getArea h) == getYMax (getArea hb)

         in if nulltest || vtest || (not vertical && htest)
                then addHint h hb >>= fillHintBoard
                else return hb

    addHintList :: HintBoard -> [Hint] -> IO HintBoard
    addHintList hb []     = return hb
    addHintList hb (h:hs) = addIf hb >>= flip addHintList hs
        where addIf = if hOrient h == hbOrient hb then addHint h else return

    addHint :: Hint -> HintBoard -> IO HintBoard
    addHint h hb = do
        pt <- nextHintPos hb
        return HintBoard
               { hints    = hints  hb++[moveTo pt h]
               , xy       = xy     hb
               , width    = width  hb
               , hbOrient = hbOrient hb
               }

    newEmptyHint :: Orientation -> [Value] -> Point -> IO Hint
    newEmptyHint o vs xy = case o of
                               Vertical   -> newVHint VHEmpty vs xy
                               Horizontal -> newHHint VHEmpty vs xy

    nextHintPos :: HintBoard -> IO Point
    nextHintPos HintBoard { hints = [], xy = xy } = return xy
    nextHintPos HintBoard
        { hints    = hs
        , xy       = xy'
        , width    = w
        , hbOrient = o
        } | o == Vertical   = nextVHintPos (last hs) xy' w
          | o == Horizontal = nextHHintPos (last hs) xy' w



    scrambleIndices :: Int -> [(Int,Int)] -> State StdGen [(Int,Int)]
    scrambleIndices 0   ijs  = return []
    scrambleIndices nHs ijs = (ijs!!) <$> state (randomR (0,length ijs-1)) >>=
                              \ij -> (ij:) <$> scrambleIndices (nHs-1) ijs

    genHintList :: Board -> [(Int,Int)] -> State StdGen
                                                 (IO ([Hint],[(Int,Int)]))
    genHintList _ []       = return $ return ([],[])
    genHintList s (ij:ijs) = do
        h  <- genHint     s ij
        hs <- genHintList s ijs
        return $ appendTouple <$> h <*> hs
        where
            appendTouple :: (a,[b]) -> ([a],[b]) -> ([a],[b])
            appendTouple (x,ys) (xs,ys') = (x:xs,ys++ys')

    genHint :: Board -> (Int,Int) -> State StdGen (IO (Hint,[(Int,Int)]))
    genHint s ij = do
        o  <- genHintOrientation
        vh <- genVHint s ij
        hh <- genHHint s ij

        return $ o >>= \o' -> if o' == Vertical then vh else hh

    removeDuplicates :: Eq a => [a] -> [a]
    removeDuplicates []     = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (/=x) xs)

    removeContained :: Eq a => [a] -> [a] -> [a]
    removeContained xs ys = foldl (flip delete) ys xs


