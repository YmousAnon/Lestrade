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
                { vali  :: Int
                , row   :: Int
                , size  :: (GLfloat,GLfloat)
                , tex   :: IO TextureObject
                , alone :: Bool
                 }

    instance Show Value where
        show Value { vali = v } = show v

    instance Eq Value where
        v == v' = vali v == vali v'

    instance Textured Value where
        draw v (x,y) = tex v >>= drawTexture (y,y') (x,x')
            where
                (x',y')
                    | alone v   = (x+w,  y+h  )
                    | otherwise = (x+w/2,y+h/2)
                (w,h) = size v


    value :: Int -> Bool -> Int -> Value
    value r a v = Value
        { vali  = v
        , row   = r
        , tex   = getValueTexture r v
        , size  = (0.2,0.2)
        , alone = a
        }

    --minSize ::
    --getValueTexture :: Int -> Int -> IO TextureObject
    --getValueTexture r v = loadTextureFromFile =<< (getVal "GRAPHICS" "tileset" >>=
    --     \ts -> return $ "res/tilesets/"++ts++"/row"++show r++"/col"++show v++".png")
    getValueTexture :: Int -> Int -> IO TextureObject
    getValueTexture r v = loadTextureFromFile =<<
        (\ts -> "res/tilesets/"++ts++"/row"++show r++"/col"++show v++".png")
            <$> getVal "GRAPHICS" "tileset"

