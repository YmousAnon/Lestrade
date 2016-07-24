module Interface.Render
(
    Renderable,
    render,
    getArea,

    display,
) where

    import Control.Monad

    import Data.IORef

    import Graphics.GLUtil
    import Graphics.Rendering.OpenGL
    import Graphics.UI.GLFW

    import Interface.Coordinate


    class Renderable a where
        render  :: Area -> a -> IO()
        getArea :: a -> Area


    display :: Renderable a => Window -> IORef Bool -> a -> IO()
    display w dirty game = readIORef dirty >>= \dirty' ->
        when dirty' $ do
           setWindowSize w (fromIntegral $ getXMax $ getArea game)
                           (fromIntegral $ getYMax $ getArea game)

           clear [ColorBuffer, DepthBuffer]
           render (getArea game) game
           swapBuffers w

           writeIORef dirty False
