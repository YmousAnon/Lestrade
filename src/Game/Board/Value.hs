module Game.Board.Value
(
    Value,
    vali,

    value,
) where
    import Graphics.GLUtil
    import Graphics.Rendering.OpenGL

    import Interface.Texture

    import Settings

    data Value = Value
                { pos   :: (GLfloat,GLfloat)
                , vali  :: Int
                , row   :: Int
                , width :: GLfloat
                , tex   :: IO TextureObject
                , alone :: Bool
                 }

    instance Show Value where
        show Value { vali = v } = show v

    instance Eq Value where
        v == v' = vali v == vali v'

    instance Textured Value where
        draw v = tex v >>= drawTexture (y,y+w) (x,x+w)
            where
                (x,y) = pos   v
                w     = if alone v then width v else width v/2


    value :: GLfloat -> Int -> Bool -> (GLfloat,GLfloat) -> Int ->
             Value
    value width r a pos v = Value
        { pos   = pos
        , vali  = v
        , row   = r
        , tex   = getValueTexture r v
        , width = width
        , alone = a
        }


    getValueTexture :: Int -> Int -> IO TextureObject
    getValueTexture r v = loadTextureFromFile =<<
        (\ts -> "res/tilesets/"++ts++"/row"++show r++"/col"++show v++".png")
            <$> getVal "GRAPHICS" "tileset"

