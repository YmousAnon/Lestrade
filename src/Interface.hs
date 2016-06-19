module Interface
(
    guiInit,

    render,
) where
    import Control.Monad

    import Data.Maybe

    import Graphics.GLUtil           as GLU
    --import Graphics.UI.GLFW          as GLFW
    import Graphics.UI.GLUT          as GLUT
    import Graphics.Rendering.OpenGL as GL

    import Settings

    import System.Exit

    import Interface.Texture

    import Game.Board.Row
    r = newRow 1 8 0.1 (-1,-1)

    guiInit :: IO GLUT.Window
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

        [r,g,b]        <- map (/255) . read <$> getVal "GRAPHICS" "bgrgb"

        --clearColor        $= Color4 1 1 1 1.0
        clearColor        $= Color4 r g b 1.0
        depthFunc         $= Just Lequal
        blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
        normalize         $= Enabled
        texture Texture2D $= Enabled
        shadeModel        $= Smooth
        displayCallback   $= render

        return w--(fromJust mw)

    --render :: Textured a => a -> DisplayCallback
    --render toDraw = do
    render :: DisplayCallback
    render = do
        clear [ColorBuffer, DepthBuffer]
        --mapM_ (\(xy,xy',tex) -> drawTexture xy xy' tex) =<< getTexture toDraw
        draw toDraw
        --color $ Color3 1 1 (1 :: GLfloat)
        flush
        where toDraw = r
