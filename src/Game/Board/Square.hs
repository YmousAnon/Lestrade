module Game.Board.Square
(
    Square,
    val,

    unsolvedSquare,
    solvedSquare,
    genSolvedSquare,

    removeVal,
    transplantSolution,
) where

    import Control.Monad.Trans.State

    import Data.List

    import Game.Board.Value

    import Interface.Coordinate
    import Interface.Input
    import Interface.Render
    import Interface.Render.Primitive

    import Settings

    import System.Random


    data Square = Solution
                { val    :: Value
                , cols   :: Int
                , row    :: Int
                , area   :: Area
                , bgrgb  :: [Double]
                , static :: Bool
                 }
               | Alternatives
                { vals   :: [Value]
                , cols   :: Int
                , row    :: Int
                , area   :: Area
                , bgrgb  :: [Double]
                , bgtile :: Value
                 }

    instance Show Square where
        show Solution     { val  = v } = show v
        show Alternatives { vals = v } = show v

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
        lclick pt Alternatives
            { vals   = vs
            , cols   = nC
            , row    = r
            , area   = a
            , bgtile = bgt
            , bgrgb  = rgb
            } = let vs' = filter (pointInArea pt . getArea) vs
                 in if null vs'
            then return Alternatives
            { vals   = vs
            , cols   = nC
            , row    = r
            , area   = a
            , bgtile = bgt
            , bgrgb  = rgb
            }
            else solvedSquare (vali $ head vs') r nC (getAreaStart a)

        lclick pt s = return s

        rclick pt Alternatives
            { vals   = vs
            , cols   = nC
            , row    = r
            , area   = a
            , bgtile = bgt
            , bgrgb  = rgb
            }
            |  not (any (pointInArea
               pt . getArea) vs) &&
               pointInArea pt a      = unsolvedSquare [1..nC] r nC $
                                                      getAreaStart a
            | otherwise              = return Alternatives
            { vals   = filter (not . pointInArea pt . getArea) vs
            , cols   = nC
            , row    = r
            , area   = a
            , bgtile = bgt
            , bgrgb  = rgb
            }

        rclick pt s = if pointInArea pt (getArea $ val s)
                      && not (static s)
            then unsolvedSquare [] (row s) (cols s) (getAreaStart $ area s)
            else return s



    unsolvedSquare :: [Int] -> Int -> Int -> Point -> IO Square
    unsolvedSquare vis r nC xy = do
        tw  <- read <$> getSetting "tileWidth"
        vs  <- mapM (uncurry (value r False) . \v ->
               (altPos nC (v-1) tw (sw tw) xy,v)) vis
        bgt <- value r True (solPos nC tw (sw tw) xy) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        return Alternatives
            { vals   = vs
            , cols   = nC
            , row    = r
            , area   = newArea xy (sw tw) (sw tw)
            , bgrgb  = bgc
            , bgtile = bgt
            }
        where sw = getSquareWidth nC

    solvedSquare :: Int -> Int -> Int -> Point -> IO Square
    solvedSquare vi r nC xy = do
        h   <- read <$> getSetting "tileWidth" :: IO Coord
        v   <- value r True (solPos nC h (sw h) xy) vi
        bgc <- map (/255) . read <$> getSetting "tilergb"

        return Solution
            { val   = v
            , cols  = nC
            , row   = r
            , area  = newArea xy (sw h) (sw h)
            , bgrgb = bgc
            , static = False
            }
        where sw = getSquareWidth nC

    genSolvedSquare :: Int -> State ([Int],StdGen) (IO Square)
    genSolvedSquare nC = state $ \(vs,g) ->
        let ig     = randomR (0, length vs-1) g
            (v,g') = (vs !! fst ig, snd ig)
            vs'    = if length vs > 1 then delete v vs else [1..nC]
            s      = solvedSquare v 1 nC (0,0)
        in  (s, (vs', g'))



    getSquareWidth :: Int -> Coord -> Coord
    getSquareWidth nC h
      | nC `mod` 2 == 0 = div h 2 * div nC 2
      | otherwise       = getSquareWidth (nC+1) h



    solPos :: Int -> Coord -> Coord -> Point -> Point
    solPos nC tw w (x,y)
        | nC < 5          = (x,y)
        | nC `mod` 2 == 0 = (x+div (tw*(nC-4)) 8,y+div (w-tw) 2)
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
            dy' = getX (solPos nC tw w (x,y))-x



    removeVal :: Value -> Square -> Square
    removeVal v Alternatives
            { vals   = vals
            , cols   = cols
            , row    = row
            , area   = area
            , bgrgb  = bgrgb
            , bgtile = bgtile
            } = Alternatives
            { vals   = delete v vals
            , cols   = cols
            , row    = row
            , area   = area
            , bgrgb  = bgrgb
            , bgtile = bgtile
            }
    removeVal v s = s

    transplantSolution :: Square -> Square -> Square
    transplantSolution s s' = Solution
                              { val    = transplantValue (getArea $ bgtile s)
                                                         (val s')
                              , cols   = cols  s
                              , row    = row   s
                              , area   = area  s
                              , bgrgb  = bgrgb s
                              , static = True
                              }
