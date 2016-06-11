module Interface.Texture
(
    drawTexture,
    loadTextureFromFile,
) where
    import Graphics.GLUtil
    import Graphics.Rendering.OpenGL
    --import Graphics.UI.GLFW

    drawTexture :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> TextureObject -> IO()
    drawTexture (x,y) (w,h) t = do

        texture Texture2D        $= Enabled
        textureBinding Texture2D $= Just t

        renderPrimitive Quads $ do
            tex 0 1 >> ver (x)     (y - h)
            tex 1 1 >> ver (x + w) (y - h)
            tex 1 0 >> ver (x + w) (y)
            tex 0 0 >> ver (x)     (y)

        texture Texture2D $= Disabled
            where ver x y = vertex (Vertex2 x y :: Vertex2 GLfloat)
                  tex u v = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)

    loadTextureFromFile :: FilePath -> IO TextureObject
    loadTextureFromFile f = do
        gt <- readTexture f
        textureFilter Texture2D $= ((Linear', Nothing), Linear')
        --textureFilter Texture2D $= ((Linear', Nothing), Linear')
        texture2DWrap $= (Mirrored, ClampToEdge)
        return $ either error id gt
