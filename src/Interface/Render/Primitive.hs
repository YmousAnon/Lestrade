module Interface.Render.Primitive
(
    renderTexture,
    loadTexture',

    renderColour,
) where

    import Graphics.UI.GLUT          as GLUT
    import Graphics.GLUtil           as GLU
    import qualified Graphics.Rendering.OpenGL as GL

    import Interface.Coordinate

    import Unsafe.Coerce


    renderTexture :: Area -> Area -> TextureObject -> IO()
    renderTexture window area tex =
        let (x,x') = xRangeToGL window $ getXRange area
            (y,y') = yRangeToGL window $ getYRange area

         in do textureBinding Texture2D $= Just tex
               renderPrimitive Quads $ do
                   col
                   txc 1 1 >> ver x' y'
                   txc 1 0 >> ver x' y
                   txc 0 0 >> ver x  y
                   txc 0 1 >> ver x  y'
        where col     = color    (Color3 1.0 1.0 1.0 :: Color3    GLfloat)
              ver x y = vertex   (Vertex2 x y        :: Vertex2   GLfloat)
              txc u v = texCoord (TexCoord2 u v      :: TexCoord2 GLfloat)

    loadTexture' :: FilePath -> IO TextureObject
    loadTexture' f = do
        tex <- either error id <$> readTexture f

        textureFilter Texture2D $= ((Linear', Nothing), Linear')

        return $ unsafeCoerce tex



    renderColour :: Area -> Area -> [Double] -> IO()
    renderColour window area rgb =
        let (x,x') = xRangeToGL window $ getXRange area
            (y,y') = yRangeToGL window $ getYRange area

         in renderPrimitive Quads $ do
                col $ map unsafeCoerce rgb
                ver x' y'
                ver x' y
                ver x  y
                ver x  y'
        where col [r,g,b] = color  (Color3 r g b :: Color3  GLfloat)
              ver x y     = vertex (Vertex2 x y  :: Vertex2 GLfloat)
