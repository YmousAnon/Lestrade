module Game.SolutionState.Loss
(
    Loss,

    newLoss,
    updateLoss,
) where

    import Graphics.Rendering.OpenGL

    import UI.Audio.Primitive
    import UI.Coordinate
    import UI.Render
    import UI.Render.Primitive

    import System.IO.Unsafe


    data Loss = Loss
                { tex  :: TextureObject
                , area :: Area
                , hymn :: IO()
                }

    instance Renderable Loss where
        render w l = renderTexture w (area l) [1,1,1] (tex l)
        getArea    = area



    newLoss :: Area -> Loss
    newLoss a = Loss
                { tex  = unsafePerformIO getTexture
                , area = a'
                , hymn = getHymn
                }
        where
            a'    = newArea (x,y) w' w'

            w'    = min w h

            x     = div (w-w') 2
            y     = div (h-w') 2

            (w,h) = getAreaEnd a

    getTexture :: IO TextureObject
    getTexture = loadTexture' "loss.png"

    getHymn :: IO()
    getHymn = loadAudio "loss.wav" >>= playAudio 1

    updateLoss :: Loss -> IO Loss
    updateLoss l = do hymn l
                      return Loss
                             { tex         = tex  l
                             , area        = area l
                             , hymn        = return()
                             }
