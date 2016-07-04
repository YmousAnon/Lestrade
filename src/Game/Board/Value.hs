module Game.Board.Value
(
    Value,
    vali,

    value,

    selectValue,
    transplantValue,
) where

    import Graphics.UI.GLUT (TextureObject)

    import Interface.Render
    import Interface.Render.Primitive

    import Interface.Coordinate

    import Settings


    data Value = Value
                { vali  :: Int
                , row   :: Int
                , tex   :: IO TextureObject
                , area  :: Area
                 }

    instance Show Value where
        show Value { vali = v } = show v

    instance Eq Value where
        v == v' = vali v == vali v'

    instance Renderable Value where
        render  w v = tex v >>= renderTexture w (area v)
        getArea     = area



    value :: Int -> Bool -> Point -> Int -> IO Value
    value r a pt v = do
        area' <- area . read <$> getSetting "tileWidth"

        return Value
            { vali  = v
            , row   = r
            , tex   = getTexture r v
            , area  = area'
            }
        where
            area tw = newArea pt (w tw) (w tw)
            w tw = if a then tw else div tw 2



    getTexture :: Int -> Int -> IO TextureObject
    getTexture r v    = loadTexture' =<< (++end) <$> root
        where
            root = (++)"res/tilesets/" <$> getSetting "tileset"
            end  = case v of
                        0 -> "/bg.png"
                        _ -> "/row"++show r++"/col"++show v++".png"




    selectValue :: Value -> [Value] -> Value
    selectValue v' (v:vs)
        | v == v'   = v
        | otherwise = selectValue v' vs

    transplantValue :: Area -> Value -> Value -> Value
    transplantValue a v v' = Value
        { vali  = vali v'
        , row   = row  v'
        , tex   = tex  v
        , area  = a
        }
