module Game.SolutionState.Loss
(
    Loss,

    newLoss,
) where

    import Graphics.Rendering.OpenGL

    import UI.Coordinate
    import UI.Render
    import UI.Render.Primitive

    import System.IO.Unsafe


    data Loss = Loss
                { tex  :: TextureObject
                , area :: Area
                }

    instance Renderable Loss where
        render w l = renderTexture w (area l) [1,1,1] (tex l)
        getArea    = area



    newLoss :: Area -> Loss
    newLoss a = Loss
                { tex  = unsafePerformIO getTexture
                , area = a'
                }
        where
            a'    = newArea (x,y) w' w'

            w'    = min w h

            x     = div (w-w') 2
            y     = div (h-w') 2

            (w,h) = getAreaEnd a

    getTexture :: IO TextureObject
    getTexture = loadTexture' "res/images/loss.png"
