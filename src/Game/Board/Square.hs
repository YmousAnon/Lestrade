module Game.Board.Square
(
    Square,

    square,
    genSolvedSquare,

    removeVal,

) where

    import Control.Monad.Extra
    import Control.Monad.Trans.State

    import Data.List

    import Game.Board.Value

    import Graphics.UI.GLFW
    import Graphics.GLUtil
    import Graphics.Rendering.OpenGL

    import Interface.Graphics
    import Interface.Texture

    import Settings

    import System.Random

    data Square = Solution
                { pos    :: (GLfloat,GLfloat)
                , val    :: Value
                , cols   :: Int
                , height :: GLfloat
                , width  :: GLfloat
                , bgrgb  :: IO [Double]
                 }
               | Alternatives
                { pos    :: (GLfloat,GLfloat)
                , vals   :: [Value]
                , cols   :: Int
                , height :: GLfloat
                , width  :: GLfloat
                , bgrgb  :: IO [Double]
                , bgtile :: Value
                 }

    instance Show Square where
        show Solution     { val  = v } = show v
        show Alternatives { vals = v } = show v

    instance Textured Square where
        --getTexture Solution     { val = v   } =            getTexture v
        --getTexture Alternatives { vals = vs } = concatMapM getTexture vs
        draw Solution
            { pos    = p
            , val    = v
            , height = h
            , width  = w
            , cols   = nC
            } = drawBG nC h w p>> draw v
        draw Alternatives
            { pos    = p
            , vals  = vs
            , height = h
            , width  = w
            , cols   = nC
            , bgtile = bgt
            } = drawBG nC h w p >> draw bgt >> mapM_ draw vs


    drawBG :: Int -> GLfloat -> GLfloat -> (GLfloat,GLfloat) -> IO()
    drawBG nC h w (x,y) = map (/255) . read <$> getVal "GRAPHICS" "tilergb" >>=
        drawSquare  (x ,x +w) (y ,y +w)
        --drawTexture (x',x'+h) (y',y'+h) bgt
        --where
        --    (x',y') = solPos nC h w (x,y)



    square :: [Int] -> Int -> Int -> GLfloat -> (GLfloat,GLfloat) ->
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
              bgc = map (/255) . read <$> getVal "GRAPHICS" "tilergb"
              --bgt = loadTextureFromFile
              --          =<< (\ts -> "res/tilesets/"++ts++"/bg.png")
              --          <$> getVal "GRAPHICS" "tileset"

    solPos :: Int -> GLfloat -> GLfloat -> (GLfloat,GLfloat) ->
              (GLfloat,GLfloat)
    solPos nC h w (x,y)
        | nC `mod` 2 == 0 = (x+h*realToFrac (fromIntegral (nC-4)/8),(w-h)/2+y)
        | otherwise       = solPos (nC+1) h w (x,y)


    altPos :: Int -> Int -> GLfloat -> GLfloat -> (GLfloat,GLfloat) ->
              (GLfloat,GLfloat)
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


    genSolvedSquare :: Int -> Int -> GLfloat -> (GLfloat,GLfloat) ->
                       State ([Int],StdGen) Square
    genSolvedSquare r nC h xy = state $ \(vs,g) ->
        let ig     = randomR (0, length vs-1) g
            (v,g') = (vs !! fst ig, snd ig)
        in  (square [v] r nC h xy, (delete v vs, g'))
