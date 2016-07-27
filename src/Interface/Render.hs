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
    import Interface.Screen


    class Renderable a where
        render  :: Area -> a -> IO()
        getArea :: a -> Area


    display :: Renderable a => Screen -> a -> IO()
    display s game = whenDirty s $ do
           resizeScreen s (getArea game)

           clear [ColorBuffer, DepthBuffer]
           render (getArea game) game
           swapBuffers' s

           cleanScreen s
