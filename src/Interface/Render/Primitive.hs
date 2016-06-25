module Interface.Render.Primitive
(
    renderTexture,
    loadTexture',

    renderColour,
) where

    --import Control.Arrow

    import Graphics.UI.GLUT          as GLUT
    import Graphics.GLUtil           as GLU
    import qualified Graphics.Rendering.OpenGL as GL

    import Interface.Coordinate

    import Unsafe.Coerce


    --renderTexture :: Area -> TextureObject -> IO()
    --renderTexture a tex = do
    --    --(x,x') <- pointToGL $ getXRange a
    --    --(y,y') <- pointToGL $ getYRange a
    --    textureBinding Texture2D $= Just tex

    --    renderPrimitive Quads $ do
    --        col
    --        txc 1 1 >> ver 1.0 0.0
    --        txc 1 0 >> ver 1.0 1.0
    --        txc 0 0 >> ver 0.0 1.0
    --        txc 0 1 >> ver 0.0 0.0
    --        --txc 1 1 >> ver y' x
    --        --txc 1 0 >> ver y' x'
    --        --txc 0 0 >> ver y  x'
    --        --txc 0 1 >> ver y  x
    --        where col     = color    (Color3 1.0 1.0 1.0 :: Color3    GLfloat)
    --              ver x y = vertex   (Vertex2 x y        :: Vertex2   GLfloat)
    --              txc u v = texCoord (TexCoord2 u v      :: TexCoord2 GLfloat)


    --renderTexture :: Area -> TextureObject -> IO()
    --renderTexture a tex = do
    --    preservingMatrix $ do
    --        rotate 90 (Vector3 1 0 0 :: Vector3 GLfloat)
    --        withTextures2D [tex] $ renderTexture' a tex

    renderTexture :: Area -> TextureObject -> IO()
    renderTexture area tex = do
        (x,x') <- xRangeToGL $ getXRange area
        (y,y') <- yRangeToGL $ getYRange area
        --putStrLn ""
        --print (show (c,b)++" - "++show (c',b'))
        --print (show (x,y)++" - "++show (x',y'))
        --putStrLn ""
        --print (x'-x,y'-y)

        putStrLn ""
        putStrLn ("x: "++show x++" - "++show x')
        putStrLn ("y: "++show y++" - "++show y')
        putStrLn ""

        textureBinding Texture2D $= Just tex
        renderPrimitive Quads $ do
            col
            txc 1 1 >> ver x' y
            txc 1 0 >> ver x' y'
            txc 0 0 >> ver x  y'
            txc 0 1 >> ver x  y
        where col     = color    (Color3 1.0 1.0 1.0 :: Color3    GLfloat)
              ver x y = vertex   (Vertex2 x y        :: Vertex2   GLfloat)
              txc u v = texCoord (TexCoord2 u v      :: TexCoord2 GLfloat)
              --(c,c') = getXRangeGL a
              --(b,b') = getYRangeGL a

    loadTexture' :: FilePath -> IO TextureObject
    loadTexture' f = do
        t <- either error id <$> readTexture f
        textureFilter Texture2D $= ((Linear', Nothing), Linear')
        texture2DWrap $= (Mirrored, ClampToEdge)
        return t
        --textureFilter Texture2D $= ((Linear', Nothing), Linear')
        --texture2DWrap $= (Mirrored, ClampToEdge)
        --get elapsedTime >>= print
        --return $ either error id $ gt
        --return $ unsafeCoerce $ either error id gt
        --return $ either error id $ unsafeCoerce gt



    renderColour :: Area -> [Double] -> IO()
    renderColour area rgb = do
        (x,x') <- xRangeToGL $ getXRange area
        (y,y') <- yRangeToGL $ getYRange area

        putStrLn ""
        putStrLn ("x: "++show x++" - "++show x')
        putStrLn ("y: "++show y++" - "++show y')
        putStrLn ""

        renderPrimitive Quads $ do
            col $ map unsafeCoerce rgb
            ver x' y
            ver x' y'
            ver x  y'
            ver x  y
            where col [r,g,b] = color  (Color3 r g b :: Color3  GLfloat)
                  ver x y     = vertex (Vertex2 x y  :: Vertex2 GLfloat)
