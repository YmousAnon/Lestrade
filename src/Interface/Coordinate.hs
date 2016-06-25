module Interface.Coordinate
(
    Coord,

    Point,
    pointToGL,

    Area,
    newArea,
    getXRange,
    getYRange,
) where

    import Graphics.UI.GLUT hiding (Point)

    import Settings

    import Unsafe.Coerce

    type Coord = Int


    xCoordToGL :: Coord -> IO GLfloat
    xCoordToGL x = do
        w <- fst <$> (read <$> getVal "screenres" :: IO Point)
        return $ (-1+fromIntegral x)/fromIntegral w

    yCoordToGL :: Coord -> IO GLfloat
    yCoordToGL y = do
        h <- snd <$> (read <$> getVal "screenres" :: IO Point)
        return $ (-1+fromIntegral y)/fromIntegral h


    type Point = (Coord,Coord)
    --instance Show Point where
    --    show (x,y) = "("++show x++","++show y++")"

    pointToGL :: Point -> IO(GLfloat,GLfloat)
    pointToGL (x,y) = do
        x' <- xCoordToGL x
        y' <- yCoordToGL y

        print (x',y')
        return (x',y')


    data Area = Area
        { xy :: Point
        , wh :: Point
        }
    instance Show Area where
        show a = show (xy a)++" - "++show (wh a)


    newArea :: Point -> Coord -> Coord -> Area
    newArea xy w h = Area { xy = xy, wh = (w,h) }



    getXRange Area { xy = (x,y), wh = (w,h) } = (x,x+w)

    getYRange :: Area -> Point
    getYRange Area { xy = (x,y), wh = (w,h) } = (y,y+h)

    --b = a >+< a
    --    where
    --        a = Coord {pixel = 12, pos = 1.0}
    --(><) P
    --class Coordinate a where
    --    (>+<) :: a -> a -> a
