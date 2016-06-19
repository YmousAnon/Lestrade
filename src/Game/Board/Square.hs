module Game.Board.Square
(
    Square,

    square,
    genSolvedSquare,

    removeVal,

) where

    --import Control.Monad.Extra
    import Control.Monad.Trans.State

    import Data.List

    import Game.Board.Value

    --import Graphics.UI.GLFW
    --import Graphics.UI.GLUT
    --import Graphics.GLUtil
    --import Graphics.Rendering.OpenGL

    import Interface.Render
    import Interface.Render.Colour

    --import Settings

    import System.Random

    data Square = Solution
                { pos    :: (Float,Float)
                , val    :: Value
                , cols   :: Int
                , height :: Float
                , width  :: Float
                , bgrgb  :: IO [Double]
                 }
               | Alternatives
                { pos    :: (Float,Float)
                , vals   :: [Value]
                , cols   :: Int
                , height :: Float
                , width  :: Float
                , bgrgb  :: IO [Double]
                , bgtile :: Value
                 }

    instance Show Square where
        show Solution     { val  = v } = show v
        show Alternatives { vals = v } = show v

    instance Renderable Square where
        --getTexture Solution     { val = v   } =            getTexture v
        --getTexture Alternatives { vals = vs } = concatMapM getTexture vs
        render Solution
            { pos    = p
            , val    = v
            , height = h
            , width  = w
            , cols   = nC
            } = renderBG nC h w p>> render v
        render Alternatives
            { pos    = p
            , vals  = vs
            , height = h
            , width  = w
            , cols   = nC
            , bgtile = bgt
            } = renderBG nC h w p >> render bgt >> mapM_ render vs


    renderBG :: Int -> Float -> Float -> (Float,Float) -> IO()
    renderBG nC h w (x,y) = return [0.0,0.0,0.0] >>=
    --renderBG nC h w (x,y) = map (/255) . read <$> getVal "GRAPHICS" "tilergb" >>=
        renderColour (x ,x +w) (y ,y +w)
        --renderTexture (x',x'+h) (y',y'+h) bgt
        --where
        --    (x',y') = solPos nC h w (x,y)



    square :: [Int] -> Int -> Int -> Float -> (Float,Float) ->
              Square
    square vs r nC h xy
        | length vs == 1 = Solution
            { pos        = xy
            , val        = value h r True (solPos nC h w xy) (head vs)
            , cols       = nC
            , height     = h
            , width      = w
            , bgrgb      = bgc
            }
        | otherwise      = Alternatives
            { pos        = xy
            , vals       = map (uncurry (value h r False)
                                . \v -> (altPos nC (v-1) h w xy,v)) vs
            , cols       = nC
            , height     = h
            , width      = w
            , bgrgb      = bgc
            , bgtile     = value h r True (solPos nC h w xy) 0
            }
        where w   = w' nC
              w' nC'
                | nC' `mod` 2 == 0 = (h/2)*realToFrac (  div nC' 2)
                | otherwise        = w' (nC'+1)
              bgc = return [0,0,0]
              --bgc = map (/255) . read <$> getVal "GRAPHICS" "tilergb"
              --bgt = loadTextureFromFile
              --          =<< (\ts -> "res/tilesets/"++ts++"/bg.png")
              --          <$> getVal "GRAPHICS" "tileset"

    solPos :: Int -> Float -> Float -> (Float,Float) -> (Float,Float)
    solPos nC h w (x,y)
        | nC `mod` 2 == 0 = (x+h*realToFrac (fromIntegral (nC-4)/8),(w-h)/2+y)
        | otherwise       = solPos (nC+1) h w (x,y)


    altPos :: Int -> Int -> Float -> Float -> (Float,Float) -> (Float,Float)
    altPos nC v h w (x,y) = (x+(h/2)*dx,(w-h)/2+y+(h/2)*dy)
        where
            dx | mod nC 2 == 0 && v <  div nC 2 ||
                 mod nC 2 == 1 && v <= div nC 2 = fromIntegral v
               | mod nC 2 == 0                  = fromIntegral (v-   div nC 2 )
               | otherwise                      = fromIntegral (v-(1+div nC 2))
                                                  +0.5
            dy | mod nC 2 == 0 && v >= div nC 2 = 0
               |                  v >  div nC 2 = 0
               | otherwise                      = 1


    removeVal :: Value -> Square -> Square
    removeVal v p = Alternatives
        { pos        = pos             p
        , vals       = delete v $ vals p
        , cols       = cols            p
        , height     = height          p
        , width      = width           p
        , bgrgb      = bgrgb           p
        , bgtile     = bgtile          p
        }


    genSolvedSquare :: Int -> Int -> Float -> (Float,Float) ->
                       State ([Int],StdGen) Square
    genSolvedSquare r nC h xy = state $ \(vs,g) ->
        let ig     = randomR (0, length vs-1) g
            (v,g') = (vs !! fst ig, snd ig)
        in  (square [v] r nC h xy, (delete v vs, g'))
