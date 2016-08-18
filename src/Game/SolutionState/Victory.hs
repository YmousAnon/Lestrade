module Game.SolutionState.Victory
(
    Victory,

    newVictory,
    updateVictory,
) where

    import Game.Board
    import Game.SolutionState.Celebration

    import Graphics.Rendering.OpenGL

    import Interface.Coordinate
    import Interface.Render
    import Interface.Render.Primitive

    import System.IO.Unsafe
    import System.Random


    data Victory = Victory
                   { tex         :: TextureObject
                   , area        :: Area
                   , celebration :: Celebration
                   }

    instance Renderable Victory where
        render w v = renderTexture w (area v) [1,1,1] (tex v) >>
                     render w (celebration v)
        getArea    = area



    newVictory :: Board -> Area -> StdGen ->  IO Victory
    newVictory s a g = do
        c <- newCelebration s a g
        return Victory
               { tex         = unsafePerformIO getTexture
               , area        = a'
               , celebration = c
               }
        where
            a'    = newArea (x,y) w' w'

            w'    = min w h

            x     = div (w-w') 2
            y     = div (h-w') 2

            (w,h) = getAreaEnd a

    getTexture :: IO TextureObject
    getTexture = loadTexture' "res/images/victory.png"

    updateVictory :: Victory -> IO Victory
    updateVictory v = do c' <- updateCelebration $ celebration v
                         print 12
                         return Victory
                                { tex         = tex            v
                                , area        = area           v
                                , celebration = c'
                                }
