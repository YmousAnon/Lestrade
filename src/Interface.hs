module Interface
(
    guiInit,
) where

    import Data.IORef

    import Graphics.GLUtil
    import Graphics.UI.GLUT hiding (GLfloat)

    import Interface.Input
    import Interface.Input.Settings
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
        keyboardMouseCallback $= Just (click ioGame)

        pushWindow

        return w
