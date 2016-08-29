module Game.SolutionState.Victory
(
    Victory,

    newVictory,
    updateVictory,
) where

    import Game.Board
    import Game.SolutionState.Celebration

    import Graphics.Rendering.OpenGL

    import System.IO.Unsafe
    import System.Random

    import UI.Audio.Primitive
    import UI.Coordinate
    import UI.Render
    import UI.Render.Primitive


    data Victory = Victory
                   { tex         :: TextureObject
                   , area        :: Area
                   , hymn        :: IO()
                   , celebration :: Celebration
                   }

    instance Renderable Victory where
        render w v = render w (celebration v) >>
                     renderTexture w (area v) [1,1,1] (tex v)
        getArea    = area



    newVictory :: Board -> Area -> StdGen ->  IO Victory
    newVictory s a g = do
        c <- newCelebration s a g
        return Victory
               { tex         = unsafePerformIO getTexture
               , area        = a'
               , hymn        = getHymn
               , celebration = c
               }
        where
            a'    = newArea (x,y) w' w'

            w'    = min w h

            x     = div (w-w') 2
            y     = div (h-w') 2

            (w,h) = getAreaEnd a

    getTexture :: IO TextureObject
    getTexture = loadTexture' "victory.png"

    getHymn :: IO()
    getHymn = loadAudio "victory.wav" >>= playAudio 1

    updateVictory :: Victory -> IO Victory
    updateVictory v = do c' <- updateCelebration $ celebration v
                         hymn v
                         return Victory
                                { tex         = tex            v
                                , area        = area           v
                                , hymn        = return()
                                , celebration = c'
                                }
