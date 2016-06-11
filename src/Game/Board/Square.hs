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

    import Interface.Texture

    import System.Random

    data Square = Solution
                { val   :: Value
                , col   :: Int
                , cols  :: Int
                , height:: GLfloat
                , width :: GLfloat
                 }
               | Alternatives
                { vals   :: [Value]
                , col    :: Int
                , cols   :: Int
                , height :: GLfloat
                , width  :: GLfloat
                 }

    instance Show Square where
        show Solution     { val  = v } = show v
        show Alternatives { vals = v } = show v

    instance Textured Square where
        draw Solution     { val  = v , height = h, cols = nC } p =
            draw  v (solPos nC h p)
        draw Alternatives { vals = vs, height = h, cols = nC } p =
            mapM_ (\v -> draw v $ altPos nC (vali v) h p) vs
 --vs

    solPos :: Int -> GLfloat -> (GLfloat,GLfloat) -> (GLfloat,GLfloat)
    solPos nC h (x,y)
        | nC `mod` 2 == 0 = (x+h*realToFrac (fromIntegral (nC-4)/8),y)
        | otherwise       = solPos (nC+1) h (x,y)


    altPos :: Int -> Int -> GLfloat -> (GLfloat,GLfloat) -> (GLfloat,GLfloat)
    altPos nC v h (x,y) = (x+(h/2)*dx,y+(h/2)*dy)
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
            , width      = w
            }
        | otherwise      = Alternatives
            { vals       = map (value r False) vs
            , col        = c
            , cols       = nC
            , height     = h
            , width      = w
            }
        where w
                | nC `mod` 2 == 0 = w*realToFrac (  div nC 2)
                | otherwise       = w*realToFrac (1+div nC 2)
        --where w
        --        | cs == 2 = 2


    removeVal :: Value -> Square -> Square
    removeVal v p = Alternatives
        { vals       = delete v $ vals p
        , col        = col             p
        , cols       = cols            p
        , height     = height          p
        , width      = width           p
        }



    genSolvedSquare :: Int -> Int -> GLfloat -> State ([Int],StdGen) Square
    genSolvedSquare r nC h = state $ \(vs,g) ->
        let ig     = randomR (0, length vs-1) g
            (v,g') = (vs !! fst ig, snd ig)
            c      = nC - length vs
        in  (square [v] r c nC h, (delete v vs, g'))
