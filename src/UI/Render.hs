module UI.Render
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

    import UI
    import UI.Coordinate


    class Renderable a where
        render  :: Area -> a -> IO()
        getArea :: a -> Area


    display :: Renderable a => UI -> a -> IO()
    display ui game = whenDirty ui $ do
           resizeWindow ui (getArea game)

           clear [ColorBuffer, DepthBuffer]
           render (getArea game) game
           swapBuffers' ui

           cleanWindow ui
