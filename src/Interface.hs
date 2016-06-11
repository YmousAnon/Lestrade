module Interface
(
    guiInit,

    --draw,
    --viewWindow,

    --initWindow,
    initGraphics,
) where
    import Data.Maybe

    import Graphics.Rendering.OpenGL as GL
    --import Graphics.UI.GLUT
    import Graphics.GLUtil
    import Unsafe.Coerce
    import Graphics.UI.GLFW
    --import Graphics.VinylGL
    --
    --import Graphics.UI.SDL.Image as SDLImage
    --import Graphics.UI.SDL.Types
    --import Graphics.UI.SDL.Video

    --import Linear (V2(..), _x, M33)

    import System.Exit

    --errorCallback :: GLFW.ErrorCallback
    --errorCallback err description = hPutStrLn stderr description

    --init :: IO Bool
    --init = GLFW.setErrorCallback (Just errorCallback)
    --    >> GLFW.init

    --a = loadGLTextureFromFile "res/Images/1.png"
    --type AppInfo = PlainRec '["cam" ::: M33 GLfloat]
    --

    -- initWindow
    --initWindow :: Size -> String -> IO ()
    --initWindow windowSize windowTitle = do
    --    _ <- getArgsAndInitialize

    --    initialWindowSize $= windowSize
    --    initialDisplayMode $= [DoubleBuffered]
    --    clearColor        $= Color4 0.9 0.1243 0.2544564 1.0

    --    _ <- createWindow windowTitle

    --    return ()

    -- initGraphics
    initGraphics :: GLdouble -> GLdouble -> IO ()
    initGraphics screenResWidth screenResHeight = do
        blend $= Enabled
        blendFunc $= (GL.SrcAlpha, OneMinusSrcAlpha)
        shadeModel $= Flat

        matrixMode $= Projection
        loadIdentity
        ortho 0.0 screenResWidth 0.0 screenResHeight (-1.0) 0.0
        matrixMode $= Modelview 0

        return ()


    --drawTexture :: (Double,Double) -> (GLsizei,GLsizei) -> TextureObject-> IO()
    --drawTexture (x,y) (w,h) tex = do
    --    texture Texture2D $= Enabled
    --    textureBinding Texture2D $= Just tex

    --    let texWidth  = fromIntegral w
    --        texHeight = fromIntegral h
    --    let texCoord2f = texCoord :: TexCoord2 GLdouble -> IO ()
    --        vertex3f  = vertex :: Vertex3 GLdouble -> IO ()
    --        color4f = color :: Color4 GLdouble -> IO ()
    --        col = color4f (Color4 (1.0::GLdouble) (1.0::GLdouble) (1.0::GLdouble) (0.0::GLdouble))
    --    let x' = toGLdouble x
    --        y' = toGLdouble y
    --    renderPrimitive Quads $ do
    --        texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 x' y' 0.0); col
    --        texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 x' (y' + texHeight) 0.0); col
    --        texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (x' + texWidth) (y' + texHeight) 0.0); col
    --        texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 (x' + texWidth) y' 0.0); col
    --    texture Texture2D $= Disabled



    guiInit :: IO Window
    guiInit = Graphics.UI.GLFW.init >>= \successfulInit ->
           if (not successfulInit) then exitFailure else return () >>

           createWindow 640 480 "Sherlock" Nothing Nothing >>= \mw ->
           case mw of
                Nothing -> (terminate >> exitFailure)
                Just w  -> makeContextCurrent mw

            -- >> clearColor        $= Color4 0.9 0.1243 0.2544564 1.0
            >> clearColor        $= Color4 0 0 0 1.0
            >> depthFunc         $= Just Lequal
            >> blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
            >> normalize         $= Enabled
            >> texture Texture2D $= Enabled
            >> shadeModel        $= Smooth

            >> return (fromJust mw)




    --toGLdouble :: a -> GLdouble
    --toGLdouble = unsafeCoerce

    --draw :: TextureObject -> IO()
    --draw tex = preservingMatrix $ withTextures2D [tex] $ do
    --    clear [ColorBuffer]
    --    textureBinding Texture2D $= Just tex
    --    renderPrimitive Quads $ do
    --        --n 1 2 0
    --        t 0 1 >> v (-1) (-1)   1
    --        t 1 1 >> v   1  (-1)   1
    --        t 1 0 >> v   1  (-1) (-1)
    --        t 0 0 >> v (-1) (-1) (-1)
    --        where v x y z = vertex (Vertex3 x y z :: Vertex3 GLfloat)
    --              n x y z = normal (Normal3 x y z :: Normal3 GLfloat)
    --              t u v   = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)


    ----viewWindow window = return() --do
    --    --(width, height) <- getFramebufferSize window
    --    --viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    --    --clear [ColorBuffer, DepthBuffer]
    --    --matrixMode $= Projection
    --    --loadIdentity
    --    --perspective 90 (fromIntegral(width)/fromIntegral(height)) 0.01 40
    --    --kmatrixMode $= Modelview 0
