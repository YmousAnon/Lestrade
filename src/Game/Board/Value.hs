module Game.Board.Value
(
    Value,
    vali,

    value,

    selectValue,
    transplantValue,

    changeCol,

    moveValueBy,
    moveValueTo,
) where

    import Graphics.Rendering.OpenGL as GL

    import Interface.Coordinate
    import Interface.Input.Settings
    import Interface.Render
    import Interface.Render.Primitive


    import System.IO.Unsafe


    data Value = Value
                { vali  :: Int
                , row   :: Int
                , tex   :: TextureObject
                , area  :: Area
                , fgrgb :: [Float]
                }

    instance Show Value where
        show Value { vali = v } = show v

    instance Eq Value where
        v == v' = vali v == vali v'

    instance Renderable Value where
        render  w v = renderTexture w (area v) (fgrgb v) (tex v)
        getArea     = area

    instance Movable Value where
        moveTo xy' v = Value
                       { vali  = vali              v
                       , row   = row               v
                       , tex   = tex               v
                       , area  = moveTo xy' $ area v
                       , fgrgb = fgrgb             v
                        }
        moveBy xy' v = Value
                       { vali  = vali              v
                       , row   = row               v
                       , tex   = tex               v
                       , area  = moveBy xy' $ area v
                       , fgrgb = fgrgb             v
                        }



    value :: Int -> Bool -> Point -> Int -> IO Value
    value r a pt v = do
        area' <- area . read <$> getSetting "tileWidth"

        return Value
            { vali  = v
            , row   = r
            , tex   = unsafePerformIO $ getTexture r v
            , area  = area'
            , fgrgb = [1,1,1]
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
        , fgrgb = [1,1,1]
        }



    changeCol :: [Float] -> Value -> Value
    changeCol fgrgb' v = Value
        { vali  = vali v
        , row   = row  v
        , tex   = tex  v
        , area  = area v
        , fgrgb = fgrgb'
        }



    moveValueBy ::  Value -> Point ->Value
    moveValueBy v (dx,dy) = moveValueTo v xy'
        where
            xy' = (x+dx,y+dy)
            (x,y) = getAreaStart $ area v

    moveValueTo :: Value -> Point -> Value
    moveValueTo v xy' = Value
        { vali  = vali                                       v
        , row   = row                                        v
        , tex   = tex                                        v
        , area  = uncurry (newArea xy') $ getAreaSize $ area v
        , fgrgb = fgrgb                                      v
        }
