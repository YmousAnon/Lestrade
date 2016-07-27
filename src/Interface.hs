module Interface
(
    guiInit,
) where

    import Control.Monad

    import Data.IORef
    import Data.Maybe

    import Graphics.GLUtil
    import Graphics.UI.GLFW as GLFW
    import Graphics.Rendering.OpenGL

    import Interface.Coordinate
    import Interface.Input
    import Interface.Input.Settings
    import Interface.Render
    import Interface.Screen

    import System.Exit

    import Data.StateVar



    guiInit :: (Clickable a,Renderable a) => a -> IO (a,Screen)
    guiInit game = do
        successfulInit <- GLFW.init
        unless successfulInit exitFailure

        mw <- createWindow (fromIntegral $ getXMax $ getArea game)
                           (fromIntegral $ getYMax $ getArea game)
                           "Sherlock" Nothing Nothing

        case mw of
            Nothing -> terminate >> exitFailure
            Just w  -> makeContextCurrent mw
        let w = fromJust mw

        dirty   <- newIORef False
        t       <- newIORef . fromJust =<< getTime
        [r,g,b] <- map (/255) . read <$> getSetting "bgrgb"

        clearColor                 $= Color4 r g b 1.0
        depthFunc                  $= Just Lequal
        blendFunc                  $= (SrcAlpha, OneMinusSrcAlpha)
        normalize                  $= Enabled
        texture Texture2D          $= Enabled
        shadeModel                 $= Smooth

        setWindowRefreshCallback w $ Just (writeDirty dirty)

        return (game,Screen w dirty t)
