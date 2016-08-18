module Interface.Render.Primitive
(
    renderColour,
    renderTexture,

    loadTexture',
) where

    import Graphics.UI.GLFW          as GLFW
    import Graphics.GLUtil           as GLU
    import Graphics.Rendering.OpenGL as GL

    import Interface.Coordinate

    import System.IO.Unsafe


    renderColour :: Area -> Area -> [Float] -> IO()
    renderColour window area rgb =
        let (x,x',y,y') = getCorners window area

         in do activeTexture $= TextureUnit 0
               textureBinding Texture2D $= Just whiteSquare
               renderPrimitive Quads $ do
                col rgb
                ver x' y'
                ver x' y
                ver x  y
                ver x  y'
        where col [r,g,b] = color  (Color3 r g b :: Color3  GLfloat)
              ver x y     = vertex (Vertex2 x y  :: Vertex2 GLfloat)

    renderTexture :: Area -> Area -> [Float] -> TextureObject -> IO()
    renderTexture window area rgb tex =
        let (x,x',y,y') = getCorners window area

         in do activeTexture $= TextureUnit 0
               textureBinding Texture2D $= Just tex
               renderPrimitive Quads $ do
                   col rgb
                   txc 1 1 >> ver x' y'
                   txc 1 0 >> ver x' y
                   txc 0 0 >> ver x  y
                   txc 0 1 >> ver x  y'
        where col [r,g,b] = color    (Color3 r g b  :: Color3    GLfloat)
              ver x y     = vertex   (Vertex2 x y   :: Vertex2   GLfloat)
              txc u v     = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)



    getCorners :: Area -> Area -> (GLfloat,GLfloat,GLfloat,GLfloat)
    getCorners window area = let (x,x') = xRangeToGL window $ getXRange area
                                 (y,y') = yRangeToGL window $ getYRange area
                              in (x,x',y,y')

    whiteSquare :: TextureObject
    whiteSquare = unsafePerformIO $ loadTexture' "res/images/white.png"

    loadTexture' :: FilePath -> IO TextureObject
    loadTexture' f = do
        tex <- either error id <$> readTexture f

        textureFilter Texture2D $= ((Linear', Nothing), Linear')

        return tex
