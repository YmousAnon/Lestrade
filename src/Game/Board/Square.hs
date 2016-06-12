module Game.Board.Square
(
    Square,
    col,

    square,
    genSolvedSquare,

    removeVal,

) where

    import Control.Monad.Trans.State

    import Data.List

    import Game.Board.Value

    import Graphics.GLUtil
    import Graphics.Rendering.OpenGL

    import Interface.Graphics
    import Interface.Texture

    import Settings

    import System.Random

    data Square = Solution
                { val    :: Value
                , col    :: Int
                , cols   :: Int
                , height :: GLfloat
                , width  :: GLfloat
                , bgrgb  :: IO [Double]
                , bgtile :: IO TextureObject
                 }
               | Alternatives
                { vals   :: [Value]
                , col    :: Int
                , cols   :: Int
                , height :: GLfloat
                , width  :: GLfloat
                , bgrgb  :: IO [Double]
                , bgtile :: IO TextureObject
                 }

    instance Show Square where
        show Solution     { val  = v } = show v
        show Alternatives { vals = v } = show v

    instance Textured Square where
        draw Solution
            { val    = v
            , height = h
            , width  = w
            , cols   = nC
            , bgtile = bgt
            } p = do
                bgt >>= drawBG nC h w p
                draw v (solPos nC h w p)

        draw Alternatives
            { vals  = vs
            , height = h
            , width  = w
            , cols   = nC
            , bgtile = bgt
            } p = do
                bgt >>= drawBG nC h w p
                mapM_ (\v -> draw v $ altPos nC (vali v) h w p) vs


    drawBG :: Int -> GLfloat -> GLfloat -> (GLfloat,GLfloat) ->
              TextureObject -> IO()
    drawBG nC h w (x,y) bgt = do
        rgb <- map (/255) . read <$> getVal "GRAPHICS" "tilergb"
        drawSquare  (x ,x +w) (y ,y +w) rgb
        drawTexture (x',x'+h) (y',y'+h) bgt
        where
            (x',y') = solPos nC h w (x,y)


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


    square :: [Int] -> Int -> Int -> Int -> GLfloat -> Square
    square vs r c nC h
        | length vs == 1 = Solution
            { val        = value r True (head vs)
            , col        = c
            , cols       = nC
            , height     = h
            , width      = w nC
            , bgrgb      = bgc
            , bgtile     = bgt
            }
        | otherwise      = Alternatives
            { vals       = map (value r False) vs
            , col        = c
            , cols       = nC
            , height     = h
            , width      = w nC
            , bgrgb      = bgc
            , bgtile     = bgt
            }
        where w nC'
                | nC' `mod` 2 == 0 = (h/2)*realToFrac (  div nC' 2)
                | otherwise        = w (nC'+1)
              bgc = map (/255) . read <$> getVal "GRAPHICS" "tilergb"
              bgt = loadTextureFromFile
                        =<< (\ts -> "res/tilesets/"++ts++"/bg.png")
                        <$> getVal "GRAPHICS" "tileset"


    removeVal :: Value -> Square -> Square
    removeVal v p = Alternatives
        { vals       = delete v $ vals p
        , col        = col             p
        , cols       = cols            p
        , height     = height          p
        , width      = width           p
        , bgrgb      = bgrgb           p
        , bgtile     = bgtile          p
        }


    genSolvedSquare :: Int -> Int -> GLfloat -> State ([Int],StdGen) Square
    genSolvedSquare r nC h = state $ \(vs,g) ->
        let ig     = randomR (0, length vs-1) g
            (v,g') = (vs !! fst ig, snd ig)
            c      = nC - length vs
        in  (square [v] r c nC h, (delete v vs, g'))
