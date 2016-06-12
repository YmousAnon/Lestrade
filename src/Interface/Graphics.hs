module Interface.Graphics
(
    drawSquare,
) where
    import Graphics.GLUtil
    import Graphics.Rendering.OpenGL

    drawSquare :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> [GLfloat] ->
                  IO()
    drawSquare (x,x') (y,y') [r,g,b] =
        renderPrimitive Quads $ do
            col
            ver y' x
            ver y' x'
            ver y  x'
            ver y  x
            where col     = color  (Color3 r g b :: Color3  GLfloat)
                  ver x y = vertex (Vertex2 x y  :: Vertex2 GLfloat)

