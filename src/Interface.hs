module Interface
(
    guiInit,
) where

    import Control.Monad

    import Data.IORef
    import Data.Maybe

    import Graphics.GLUtil
    import Graphics.UI.GLUT hiding (GLfloat)

    import Settings

    import System.Exit

    import Interface.Input
    import Interface.Render


    guiInit :: (Clickable a,Renderable a) => IORef a -> IO Window
    guiInit ioGame = do
        (_progName, _args) <- getArgsAndInitialize
        w                  <- createWindow "Sherlock"
        [r,g,b]            <- map (/255) . read <$> getSetting "bgrgb"

        clearColor            $= Color4 r g b 1.0
        depthFunc             $= Just Lequal
        blendFunc             $= (SrcAlpha, OneMinusSrcAlpha)
        normalize             $= Enabled
        texture Texture2D     $= Enabled
        shadeModel            $= Smooth
        displayCallback       $= display ioGame
        keyboardMouseCallback $= (Just (click ioGame))

        pushWindow

        return w
