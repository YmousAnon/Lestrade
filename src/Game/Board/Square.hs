module Game.Board.Square
(
    Square,
    val,

    unsolvedSquare,
    solvedSquare,
    genSolvedSquare,

    getSolution,

    removeVal,
    transplantSolution,
) where

    import Control.Monad
    import Control.Monad.Trans.State

    import Data.List

    import Game.Board.Value
    import Game.SolutionState

    import UI
    import UI.Coordinate
    import UI.Input
    import UI.Input.Settings
    import UI.Render
    import UI.Render.Primitive
    import UI.Audio

    import System.Random


    data Square = Solution
                { val    :: Value
                , cols   :: Int
                , row    :: Int
                , area   :: Area
                , bgrgb  :: [Float]
                , static :: Bool
                 }
               | Alternatives
                { allVals :: [Value]
                , vals    :: [Value]
                , cols    :: Int
                , row     :: Int
                , area    :: Area
                , bgrgb   :: [Float]
                , bgtile  :: Value
                 }

    instance Eq Square where
        Solution     { val  = v } == Solution     { val  = v' } = v == v'
        Alternatives { vals = v } == Alternatives { vals = v' } = v == v'
        Solution     { val  = v } == Alternatives { vals = v' } = False
        Alternatives { vals = v } == Solution     { val  = v' } = False

    instance Renderable Square where
        getArea = area
        render w Solution
            { val    = v
            , area   = a
            , cols   = nC
            , bgrgb  = rgb
            } = renderColour w a rgb
             >> render w v

        render w Alternatives
            { vals   = vs
            , area   = a
            , bgtile = bgt
            , bgrgb  = rgb
            } = renderColour w a rgb
             >> render w bgt
             >> mapM_ (render w) vs

    instance Clickable Square where
        lclick ui pt Alternatives
            { allVals = avs
            , vals    = vs
            , cols    = nC
            , row     = r
            , area    = a
            , bgtile  = bgt
            , bgrgb   = rgb
            } = let vs' = filter (pointInArea pt . getArea) vs
                 in if null vs'
            then return Alternatives
            { allVals = avs
            , vals    = vs
            , cols    = nC
            , row     = r
            , area    = a
            , bgtile  = bgt
            , bgrgb   = rgb
            }
            else playMainClick ui >>
                 solvedSquare (vali $ head vs') r nC (getAreaStart a)

        lclick ui pt s = return s

        rclick ui pt Alternatives
            { allVals = avs
            , vals    = vs
            , cols    = nC
            , row     = r
            , area    = a
            , bgtile  = bgt
            , bgrgb   = rgb
            } = when (vs/=vs') (playSecondaryClick ui) >>
                return Alternatives
            { allVals = avs
            , vals    = vs'
            , cols    = nC
            , row     = r
            , area    = a
            , bgtile  = bgt
            , bgrgb   = rgb
            }
            where
                dv = filter (pointInArea pt . getArea) vs
                av = filter (pointInArea pt . getArea) avs

                vs' | null av   = vs
                    | null dv   = sort $ av++vs
                    | otherwise = delete (head dv) vs

        rclick ui pt s = if pointInArea pt (getArea $ val s) && not (static s)
            then do nC <- read <$> getSetting "columns"
                    playSecondaryClick ui
                    unsolvedSquare [1..nC] (row s) (cols s)
                                   (getAreaStart $ area s)
            else return s

    instance Solvable Square where
        Alternatives{}     |-|_                     = UnSolved
        _                  |-|Alternatives{}        = UnSolved
        Solution{ val = v }|-|Solution{ val  = v' } | v == v'   = Correct
                                                    | otherwise = Wrong



    unsolvedSquare :: [Int] -> Int -> Int -> Point -> IO Square
    unsolvedSquare vis r nC xy = do
        tw  <- read <$> getSetting "tileWidth"
        let (ssX,ssY) = getSquareSize nC tw
        vs  <- mapM (uncurry (value r False) . \v ->
               (altPos nC (v-1) tw ssX xy,v)) vis
        bgt <- value r True (solPos nC tw ssX xy) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        return Alternatives
            { allVals = vs
            , vals    = vs
            , cols    = nC
            , row     = r
            , area    = newArea xy ssX ssY
            , bgrgb   = bgc
            , bgtile  = bgt
            }

    solvedSquare :: Int -> Int -> Int -> Point -> IO Square
    solvedSquare vi r nC xy = do
        tW  <- read <$> getSetting "tileWidth"
        let (ssX,ssY) = getSquareSize nC tW
        v   <- value r True (solPos nC tW ssX xy) vi
        bgc <- map (/255) . read <$> getSetting "tilergb"

        return Solution
            { val   = v
            , cols  = nC
            , row   = r
            , area  = newArea xy ssX ssY
            , bgrgb = bgc
            , static = False
            }

    genSolvedSquare :: Int -> Int -> State ([Int],StdGen) (IO Square)
    genSolvedSquare nC r = state $ \(vs,g) ->
        let ig     = randomR (0, length vs-1) g
            (v,g') = (vs !! fst ig, snd ig)
            vs'    = if length vs > 1 then delete v vs else [1..nC]
            s      = solvedSquare v r nC (0,0)
        in  (s, (vs', g'))



    getSolution :: Square -> Maybe Value
    getSolution Solution{ val = v } = Just v
    getSolution _                   = Nothing



    getSquareSize :: Int -> Coord -> Point
    getSquareSize nC tW = (squareSizeX, squareSizeY)
        where
            squareSizeX
                | nC `mod` 2 == 0 = div tW 2 * div nC 2
                | otherwise       = fst $ getSquareSize (nC+1) tW

            squareSizeY
                | nC `mod` 2 == 0 = tW + div (squareSizeX -tW) 2
                | otherwise       = snd $ getSquareSize (nC+1) tW

    solPos :: Int -> Coord -> Coord -> Point -> Point
    solPos nC tw w (x,y)
        | nC < 5          = (x,y)
        | nC `mod` 2 == 0 = (x+div (tw*(nC-4)) 8,y+div (w-tw) 4)
        | otherwise       = solPos (nC+1) tw w (x,y)

    altPos :: Int -> Int -> Coord -> Coord -> Point -> Point
    altPos nC v tw w (x,y) = (x + div tw 4 *dx, y + dy + dy')
        where
            dx | nC < 3                         = 1
               | mod nC 2 == 0 && v <  div nC 2 ||
                 mod nC 2 == 1 && v <= div nC 2 = 2*v
               | mod nC 2 == 0                  = 2*(v-div nC 2)
               | otherwise                      = 2*(v-div nC 2)-1
            dy | mod nC 2 == 0 && v >= div nC 2 = div tw 2
               |                  v >  div nC 2 = div tw 2
               | otherwise                      = 0
            dy' = getY (solPos nC tw w (x,y))-y



    removeVal :: Value -> Square -> Square
    removeVal v Alternatives
            { allVals = avs
            , vals    = vs
            , cols    = nC
            , row     = r
            , area    = a
            , bgrgb   = bgrgb
            , bgtile  = bgtile
            } = Alternatives
            { allVals = avs
            , vals    = delete v vs
            , cols    = nC
            , row     = r
            , area    = a
            , bgrgb   = bgrgb
            , bgtile  = bgtile
            }
    removeVal v s = s

    transplantSolution :: Square -> Square -> Square
    transplantSolution s s' = Solution
                              { val    = transplantValue (getArea $ bgtile s)
                                                         (selectValue (val  s')
                                                                      (vals s))
                                                         (val s')
                              , cols   = cols  s
                              , row    = row   s
                              , area   = area  s
                              , bgrgb  = bgrgb s
                              , static = True
                              }
