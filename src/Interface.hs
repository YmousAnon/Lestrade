module Interface
(
    guiInit,
) where
    import Control.Monad

    import Data.Maybe
    import Data.IORef

    import Graphics.GLUtil
    --import Graphics.UI.GLFW          as GLFW
    import Graphics.UI.GLUT hiding (GLfloat)
    --import Graphics.Rendering.OpenGL hiding (Window)

    import Settings

    import System.Exit

    import Interface.Render

    --import Game.Board
    --import Game.Board.Row

    guiInit :: Renderable a => IORef a -> IO Window
    --guiInit :: Renderable a => IORef a -> IO Window
    guiInit ioGame = do
        --successfulInit <- init
        (_progName, _args) <- getArgsAndInitialize
        --unless successfulInit exitFailure

        --(w,h)          <- read <$> getSetting "GRAPHICS" "screenres"
        w                  <- createWindow "Sherlock"
        --case mw of
        --    Nothing -> exitFailure
        --    --Nothing -> terminate >> exitFailure
        --    Just w  -> makeContextCurrent mw

        [r,g,b]            <- map (/255) . read <$> getSetting "bgrgb"

        --clearColor        $= Color4 1 1 1 1.0
        clearColor        $= Color4 r g b 1.0
        depthFunc         $= Just Lequal
        blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
        normalize         $= Enabled
        texture Texture2D $= Enabled
        shadeModel        $= Smooth
        displayCallback   $= (display ioGame)
        --clearColor $= Color4 0.9 0.1243 0.2544564 1.0
        --depthFunc $= Just Lequal
        --blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        --normalize $= Enabled
        --texture Texture2D $= Enabled
        --shadeModel $= Smooth
        pushWindow

        return w--(fromJust mw)
        --where  -- :: IO (IORef Int)
        --where display' = (display b :: DisplayCallback)

    --render :: Textured a => a -> DisplayCallback
    --render toDraw = do
