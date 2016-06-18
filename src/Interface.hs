module Interface
(
    guiInit,
) where
    import Control.Monad

    import Data.Maybe

    import Graphics.GLUtil           as GLU
    import Graphics.UI.GLFW          as GLFW
    import Graphics.Rendering.OpenGL as GL

    import Settings

    import System.Exit



    guiInit :: IO Window
    guiInit = do
        successfulInit <- GLFW.init
        unless successfulInit exitFailure

        (w,h)          <- read <$> getVal "GRAPHICS" "screenres"
        mw             <- createWindow w h "Sherlock" Nothing Nothing
        case mw of
            Nothing -> terminate >> exitFailure
            Just w  -> makeContextCurrent mw

        [r,g,b]        <- map (/255) . read <$> getVal "GRAPHICS" "bgrgb"

        clearColor        $= Color4 r g b 1.0
        depthFunc         $= Just Lequal
        blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
        normalize         $= Enabled
        texture Texture2D $= Enabled
        shadeModel        $= Smooth

        return (fromJust mw)
