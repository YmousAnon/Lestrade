module UI.Init
(
    uiInit,
) where

    import Control.Monad

    import Data.IORef
    import Data.Maybe

    import Graphics.GLUtil
    import Graphics.UI.GLFW          as GLFW
    import Graphics.Rendering.OpenGL

    import Sound.ALUT
    import System.Exit

    import UI
    import UI.Coordinate
    import UI.Input
    import UI.Input.Settings
    import UI.Render

    import Data.StateVar


    uiInit :: (Clickable a,Renderable a) => (a -> UI -> IO()) -> a -> IO()
    uiInit loop game = withProgNameAndArgs runALUT $ \_ _ -> do
        successfulInit <- GLFW.init
        unless successfulInit exitFailure

        mw      <- createWindow (fromIntegral $ getXMax $ getArea game)
                                (fromIntegral $ getYMax $ getArea game)
                                "Lestrade" Nothing Nothing
        dirty   <- newIORef False
        t       <- newIORef . fromJust =<< getTime

        case mw of
            Nothing -> terminate >> exitFailure
            Just w  -> makeContextCurrent mw
        let w = fromJust mw

        setWindowRefreshCallback w $ Just (writeDirty dirty)

        [r,g,b] <- map (/255) . read <$> getSetting "bgrgb"

        blend                      $= Enabled
        blendFunc                  $= (SrcAlpha, OneMinusSrcAlpha)
        clearColor                 $= Color4 r g b 1.0
        depthFunc                  $= Just Lequal
        normalize                  $= Enabled
        shadeModel                 $= Smooth
        texture Texture2D          $= Enabled

        loop game $ UI w dirty t
