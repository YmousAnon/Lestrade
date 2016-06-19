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
    import Interface.Render.Texture

    import Settings

    --bms = mapM (mapM loadBM) rcs
    --bms = mapM (mapM loadBM) rcs
    ----bms = (sequence . map) ((sequence . map) loadBM) rcs
    --    where
    --        root         = (++)"res/tilesets/" <$> getVal "GRAPHICS" "tileset"
    --        end r c      = "/row"++show r++"/col"++show c++".png"
    --        loadBM (r,c) = root >>= \root' ->
    --                       loadTextureFromFile (root'++end r c)
    --        rcs          = [[(r,c) | r <- [1..8]]
    --                               | c <- [1..8]]

    data Value = Value
                { pos   :: (Float,Float)
                , vali  :: Int
                , row   :: Int
                , width :: Float
                , tex   :: IO TextureObject
                , alone :: Bool
                 }

    instance Show Value where
        show Value { vali = v } = show v

    instance Eq Value where
        v == v' = vali v == vali v'

    instance Renderable Value where
        --getTexture v = (\v' -> [((x,x+w),(y,y+w),v')]) <$>  tex v
        render v = tex v >>= renderTexture (y,y+w) (x,x+w)
            where
                (x,y) = pos   v
                w     = if alone v then width v else width v/2
                r     = row  v
                vi    = vali v
        --draw v = tex v >>= drawTexture (y,y+w) (x,x+w)
        --draw v
        --    | vi == 0   = return()
        --    | otherwise = (((!! (r-1)) . (!! (vi-1))) <$> bms) >>= drawTexture (y,y+w) (x,x+w)
        --draw v = putStrLn ("("++show vi++", "++show r++")")-- >>
        --         >> (((!! vi) . (!! r)) <$> bms) >>= print
        --draw v = putStrLn ("("++show vi++", "++show r++")")-- >>
            --(((!! vi) . (!! r)) <$> bms) >>= drawTexture (y,y+w) (x,x+w)
        --draw v = tex v >>= drawTexture (y,y+w) (x,x+w)


    value :: Float -> Int -> Bool -> (Float,Float) -> Int ->
             Value
    value width r a pos v = Value
        { pos   = pos
        , vali  = v
        , row   = r
        , tex   = getTexture r v
        , width = width
        , alone = a
        }


    getTexture :: Int -> Int -> IO TextureObject
    getTexture r v    = loadTexture' =<< (\root' -> root'++end) <$> root
        where
            root = (++)"res/tilesets/" <$> getVal "tileset"
            end  = case v of
                        0 -> "/bg.png"
                        _ -> "/row"++show r++"/col"++show v++".png"

