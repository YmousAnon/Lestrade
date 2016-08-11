module Game.HintBoard.Decoration
(
    DecorationType(Spear,Inversion),

    Decoration,
    newDecoration,

    changeDecorationColour,
) where

    import Graphics.Rendering.OpenGL

    import Interface.Coordinate
    import Interface.Input.Settings
    import Interface.Render
    import Interface.Render.Primitive

    import System.IO.Unsafe


    data DecorationType = Spear | Inversion



    data Decoration = Decoration
                      { tex   :: TextureObject
                      , area  :: Area
                      , fgrgb :: [Float]
                      }

    instance Renderable Decoration where
        render  w d = renderTexture w (area d) (fgrgb d) (tex d)
        getArea     = area

    instance Movable Decoration where
        moveTo xy' d = Decoration
                       { tex   = tex               d
                       , area  = moveTo xy' $ area d
                       , fgrgb = fgrgb             d
                       }
        moveBy xy' d = Decoration
                       { tex   = tex               d
                       , area  = moveBy xy' $ area d
                       , fgrgb = fgrgb             d
                       }



    newDecoration :: Area -> DecorationType -> IO Decoration
    newDecoration a dt = return Decoration
         { tex   = unsafePerformIO $ getTexture dt
         , area  = a
         , fgrgb = [1,1,1]
         }

    getTexture :: DecorationType -> IO TextureObject
    getTexture dt = loadTexture' =<< (++end) <$> root
        where
            root = (++"/decorations/") . ("res/tilesets/"++)
                   <$> getSetting "tileset"

            end = case dt of
                      Spear     -> "spear.png"
                      Inversion -> "inversion.png"



    changeDecorationColour :: [Float] -> Decoration -> Decoration
    changeDecorationColour fgrgb' d = Decoration
         { tex   = tex d
         , area  = area d
         , fgrgb = fgrgb'
         }
