module Interface
(
    guiInit,
) where
    import Control.Monad

    import Data.Maybe

    import Graphics.GLUtil
    --import Graphics.UI.GLFW          as GLFW
    import Graphics.UI.GLUT hiding (GLfloat)
    --import Graphics.Rendering.OpenGL hiding (Window)

    import Settings

    import System.Exit

    import Interface.Render

    import Game.Board.Row
    r = newRow 1 8 0.1 (-1,-1)

    guiInit :: IO Window
    guiInit = do
        --successfulInit <- init
        (_progName, _args) <- getArgsAndInitialize
        --unless successfulInit exitFailure

        --(w,h)          <- read <$> getVal "GRAPHICS" "screenres"
        w              <- createWindow "Sherlock"
        --case mw of
        --    Nothing -> exitFailure
        --    --Nothing -> terminate >> exitFailure
        --    Just w  -> makeContextCurrent mw

        [r,g,b]        <- map (/255) . read <$> getVal "bgrgb"

        --clearColor        $= Color4 1 1 1 1.0
        clearColor        $= Color4 r g b 1.0
        depthFunc         $= Just Lequal
        blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
        normalize         $= Enabled
        texture Texture2D $= Enabled
        shadeModel        $= Smooth
        displayCallback   $= display'

        return w--(fromJust mw)
        where display' = (display r :: DisplayCallback)

    --render :: Textured a => a -> DisplayCallback
    --render toDraw = do
