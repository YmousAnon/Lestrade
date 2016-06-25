module Interface.Render
(
    Renderable,
    render,

    display,
) where
    import Data.IORef

    import Graphics.UI.GLUT
    import Graphics.GLUtil

    import Interface.Coordinate
    --import Graphics.Rendering.OpenGL

    --import Game.Board.Row
    --r = newRow 1 8 0.1 (-1,-1)

    class Renderable a where
        render :: a -> IO()


    --sizeX = 1200
    --sizeY = 800
    --size  = newArea (0,0) sizeX sizeY

    display :: Renderable a => IORef a -> DisplayCallback
    display ioGame = do
        clear [ColorBuffer, DepthBuffer]

        readIORef ioGame >>= render
        get elapsedTime >>= print

        windowSize $= Size 1200 800
        flush
