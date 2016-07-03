module Interface.Render
(
    Renderable,
    render,
    getArea,

    display,
) where

    import Data.IORef
    import Data.StateVar

    import Graphics.GLUtil
    import Graphics.UI.GLUT

    import Interface.Coordinate


    class Renderable a where
        render  :: Area -> a -> IO()
        getArea :: a -> Area


    display :: Renderable a => IORef a -> DisplayCallback
    display ioGame = do
        clear [ColorBuffer, DepthBuffer]

        game <- readIORef ioGame

        windowSize $= Size (fromIntegral $ getXMax $ getArea game)
                           (fromIntegral $ getYMax $ getArea game)
        render (getArea game) game

        flush
