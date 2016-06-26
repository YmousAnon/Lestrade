module Game.Board.Square
(
    Square,

    unsolvedSquare,
    solvedSquare,
    genSolvedSquare,

    removeVal,

) where

    --import Control.Monad.Extra
    import Control.Monad.Trans.State

    import Data.List

    import Settings

    import Game.Board.Value

    --import Graphics.UI.GLFW
    --import Graphics.UI.GLUT
    --import Graphics.GLUtil
    --import Graphics.Rendering.OpenGL

    import Interface.Coordinate
    import Interface.Render
    import Interface.Render.Primitive

    --import Settings

    import System.Random

    data Square = Solution
                { val    :: Value
                , cols   :: Int
                , area   :: Area
                , bgrgb  :: [Double]
                 }
               | Alternatives
                { vals   :: [Value]
                , cols   :: Int
                , area   :: Area
                , bgrgb  :: [Double]
                , bgtile :: Value
                 }

    instance Show Square where
        show Solution     { val  = v } = show v
        show Alternatives { vals = v } = show v

    instance Renderable Square where
        render Solution
            { val    = v
            , area   = a
            , cols   = nC
            , bgrgb  = rgb
            } = renderColour a rgb >> render v
        render Alternatives
            { vals   = vs
            , area   = a
            , bgtile = bgt
            , bgrgb  = rgb
            } = renderColour a rgb >> render bgt >> mapM_ render vs


    unsolvedSquare :: [Int] -> Int -> Int -> Point -> IO Square
    unsolvedSquare vis r nC xy = do
        tw  <- read <$> getSetting "tileWidth"
        vs  <- mapM (uncurry (value r False) . \v ->
               (altPos nC (v-1) tw (w tw) xy,v)) vis
        bgt <- value r True (solPos nC tw (w tw) xy) 0
        bgc <- map (/255) . read <$> getSetting "tilergb"

        return Alternatives
            { vals   = vs
            , cols   = nC
            , area   = newArea xy (w tw) (w tw)
            , bgrgb  = bgc
            , bgtile = bgt
            }
        where w = w' nC :: Coord -> Coord
              --w = w' nC :: Coord -> Coord
              w' :: Int -> Coord -> Coord
              w' nC' h'
                | nC' `mod` 2 == 0 = (div h' 2)*(  div nC' 2)
                | otherwise        = w' (nC'+1) h'

    solvedSquare :: Int -> Int -> Int -> Point -> IO Square
    solvedSquare vi r nC xy = do
        h   <- read <$> getSetting "tileWidth" :: IO Coord
        --vs  <- mapM (uncurry (value r False) . \v ->
        --       (altPos nC (v-1) h (w h) xy,v)) vs
        v   <- value r True (solPos nC h (w h) xy) vi
        bgc <- map (/255) . read <$> getSetting "tilergb"
        return Solution
            { val   = v
            , cols  = nC
            , area  = newArea xy (w h) (w h)
            , bgrgb = bgc
            }
        where w = w' nC :: Coord -> Coord
              w' :: Int -> Coord -> Coord
              w' nC' h'
                | nC' `mod` 2 == 0 = (div h' 2)*(  div nC' 2)
                | otherwise        = w' (nC'+1) h'

    genSolvedSquare :: Int -> Int -> Point -> State ([Int],StdGen) (IO Square)
    genSolvedSquare r nC xy = state $ \(vs,g) ->
        let ig     = randomR (0, length vs-1) g
            (v,g') = (vs !! fst ig, snd ig)
        in  (solvedSquare v r nC xy, (delete v vs, g'))
    --square :: [Int] -> Int -> Int -> Point -> IO Square
    --square vs r nC xy
    --    | length vs == 1 = do
    --        h <- read <$> getSetting "tileWidth"
    --        Solution
    --            { val        = value h r True (solPos nC h w xy) (head vs)
    --            , cols       = nC
    --            , area       = newArea xy (w h) (w h)
    --            , bgrgb      = bgc
    --            }
    --    | otherwise      = do
    --        h   <- read <$> getSetting "tileWidth" :: IO Coord
    --        vs  <- mapM (uncurry (value h r False)
    --               . \v -> (altPos nC (v-1) h w xy,v)) vs
    --        bgt <- value (solPos nC h (w h) xy) r True 0

    --        return Alternatives
    --            { vals   = vs
    --            , cols   = nC
    --            , area   = newArea xy (w h) (w h)
    --            , bgrgb  = bgc
    --            , bgtile = bgt
    --            }
    --    where w = w' nC :: Coord -> Coord
    --          w' :: Int -> Coord -> Coord
    --          w' nC' h'
    --            | nC' `mod` 2 == 0 = (h'/2)*realToFrac (  div nC' 2)
    --            | otherwise        = w' (nC'+1) h'
    --          bgc = return [0,0,0]
    --          --bgc = map (/255) . read <$> getSetting "GRAPHICS" "tilergb"
    --          --bgt = loadTextureFromFile
    --          --          =<< (\ts -> "res/tilesets/"++ts++"/bg.png")
    --          --          <$> getSetting "GRAPHICS" "tileset"

    solPos :: Int -> Coord -> Coord -> Point -> Point
    solPos nC tw w (x,y)
        | nC < 5          = (x,y)
        | nC `mod` 2 == 0 = (x+div (tw*(nC-4)) 8,y+div (w-tw) 2)
        | otherwise       = solPos (nC+1) tw w (x,y)

    altPos :: Int -> Int -> Coord -> Coord -> Point -> Point
    altPos nC v tw w (x,y) = (x + (div tw 4)*dx, y + dy + dy')
        where
            dx | nC < 3                         = 1
               | mod nC 2 == 0 && v <  div nC 2 ||
                 mod nC 2 == 1 && v <= div nC 2 = 2*v
               | mod nC 2 == 0                  = 2*(v-div nC 2)
               | otherwise                      = 2*(v-div nC 2)-1
            dy | mod nC 2 == 0 && v >= div nC 2 = 0
               |                  v >  div nC 2 = 0
               | otherwise                      = (div tw 2)
            dy' :: Coord
            dy' = (getX $ solPos nC tw w (x,y))-x
            --dy'k
               -- | mod nC 2 == 0 && v <  div nC 2 ||
               --   mod nC 2 == 1 && v <= div nC 2 = fromIntegral v
               -- | mod nC 2 == 0                  = fromIntegral (v-   div nC 2 )
               -- | otherwise                      = fromIntegral (v-(1+div nC 2))
    --    (x + (div h 2)*round dx, y + (div (w-h) 2)*round dy)
    --    where
    --        dx | mod nC 2 == 0 && v <  div nC 2 ||
    --             mod nC 2 == 1 && v <= div nC 2 = fromIntegral v
    --           | mod nC 2 == 0                  = fromIntegral (v-   div nC 2 )
    --           | otherwise                      = fromIntegral (v-(1+div nC 2))
    --                                              +0.5
    --        dy | mod nC 2 == 0 && v >= div nC 2 = 0
    --           |                  v >  div nC 2 = 0
    --           | otherwise                      = 1




    --altPos :: Int -> Int -> Coord -> Coord -> Point -> Point
    --altPos nC v h w (x,y) = --(x+(div h 2)*dx,(w-h)/2+y+(div h 2)*dy)
    --    (x + (div h 2)*round dx, y + (div (w-h) 2)*round dy)
    --    where
    --        dx | mod nC 2 == 0 && v <  div nC 2 ||
    --             mod nC 2 == 1 && v <= div nC 2 = fromIntegral v
    --           | mod nC 2 == 0                  = fromIntegral (v-   div nC 2 )
    --           | otherwise                      = fromIntegral (v-(1+div nC 2))
    --                                              +0.5
    --        dy | mod nC 2 == 0 && v >= div nC 2 = 0
    --           |                  v >  div nC 2 = 0
    --           | otherwise                      = 1


    removeVal :: Value -> Square -> Square
    removeVal v p = Alternatives
        { vals       = delete v $ vals p
        , cols       = cols            p
        , area       = area            p
        , bgrgb      = bgrgb           p
        , bgtile     = bgtile          p
        }


