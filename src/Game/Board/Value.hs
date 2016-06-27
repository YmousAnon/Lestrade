module Game.Board.Value
(
    Value,
    vali,

    value,
) where
    import Graphics.UI.GLUT (TextureObject)
    --import Graphics.GLUtil
    --import Graphics.Rendering.OpenGL as GL (TextureObject)

    import Interface.Render
    import Interface.Render.Primitive

    import Interface.Coordinate

    import Settings

    --bms = mapM (mapM loadBM) rcs
    --bms = mapM (mapM loadBM) rcs
    ----bms = (sequence . map) ((sequence . map) loadBM) rcs
    --    where
    --        root         = (++)"res/tilesets/" <$> getSetting "GRAPHICS" "tileset"
    --        end r c      = "/row"++show r++"/col"++show c++".png"
    --        loadBM (r,c) = root >>= \root' ->
    --                       loadTextureFromFile (root'++end r c)
    --        rcs          = [[(r,c) | r <- [1..8]]
    --                               | c <- [1..8]]

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
        --getTexture v = (\v' -> [((x,x+w),(y,y+w),v')]) <$>  tex v
        render  v = tex v >>= renderTexture (area v)
        getArea v = area v
        --render v = print (area v) >> tex v >>= renderTexture (area v)
        --draw v = tex v >>= drawTexture (y,y+w) (x,x+w)
        --draw v
        --    | vi == 0   = return()
        --    | otherwise = (((!! (r-1)) . (!! (vi-1))) <$> bms) >>= drawTexture (y,y+w) (x,x+w)
        --draw v = putStrLn ("("++show vi++", "++show r++")")-- >>
        --         >> (((!! vi) . (!! r)) <$> bms) >>= print
        --draw v = putStrLn ("("++show vi++", "++show r++")")-- >>
            --(((!! vi) . (!! r)) <$> bms) >>= drawTexture (y,y+w) (x,x+w)
        --draw v = tex v >>= drawTexture (y,y+w) (x,x+w)


    value :: Int -> Bool -> Point -> Int -> IO Value
    value r a pt v = do
        area' <- area <$> read <$> getSetting "tileWidth"

        return Value
            { vali  = v
            , row   = r
            , tex   = getTexture r v
            , area  = area'
            }
        where
            --tex  =

            area tw = newArea pt (w tw) (w tw)
            w tw = if a then tw else div tw 2


    getTexture :: Int -> Int -> IO TextureObject
    getTexture r v    = loadTexture' =<< (\root' -> root'++end) <$> root
        where
            root = (++)"res/tilesets/" <$> getSetting "tileset"
            end  = case v of
                        0 -> "/bg.png"
                        _ -> "/row"++show r++"/col"++show v++".png"
