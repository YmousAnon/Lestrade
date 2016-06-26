module Interface.Coordinate
(
    Coord,

    Point,
    getX,
    getY,
    pointToGL,

    Area,
    newArea,
    --getPos,
    --getSize,
    getXRange,
    getYRange,
    xRangeToGL,
    yRangeToGL,
) where

    import Graphics.UI.GLUT hiding (Point)

    import Settings

    import Unsafe.Coerce

    type Coord = Int


    xCoordToGL :: Coord -> IO GLfloat
    xCoordToGL x = do
        w <- fst <$> (read <$> getSetting "screenres" :: IO Point)
        return $ (fromIntegral x/fromIntegral w)-1

    yCoordToGL :: Coord -> IO GLfloat
    yCoordToGL y = do
        h <- snd <$> (read <$> getSetting "screenres" :: IO Point)
        return $ (fromIntegral y/fromIntegral h)-1


    type Point = (Coord,Coord)

    getX :: Point -> Coord
    getX (x,y) = x

    getY :: Point -> Coord
    getY (x,y) = y

    pointToGL :: Point -> IO(GLfloat,GLfloat)
    pointToGL (x,y) = do
        x' <- xCoordToGL x
        y' <- yCoordToGL y

        --putStrLn ""
        --putStrLn ("x: "++show x++" - "++show x')
        --putStrLn ("y: "++show y++" - "++show y')
        --putStrLn ""
        --putStrLn ("x: "++show h)
        return (x',y')


    data Area = Area
        { xy :: Point
        , wh :: Point
        }
    instance Show Area where
        show a = show (x,y)++" - "++show (x+w,x+y)
            where (x,y) = xy a
                  (w,h) = wh a



    newArea :: Point -> Coord -> Coord -> Area
    newArea xy w h = Area { xy = xy, wh = (w,h) }



    getXRange :: Area -> Point
    getXRange Area { xy = (x,y), wh = (w,h) } = (x,x+w)

    getYRange :: Area -> Point
    getYRange Area { xy = (x,y), wh = (w,h) } = (y,y+h)


    xRangeToGL :: Point -> IO(GLfloat,GLfloat)
    xRangeToGL (x0,x1) = do
        x0' <- xCoordToGL x0
        x1' <- xCoordToGL x1

        return (x0',x1')

    yRangeToGL :: Point -> IO(GLfloat,GLfloat)
    yRangeToGL (y0,y1) = do
        y0' <- yCoordToGL y0
        y1' <- yCoordToGL y1

        return (y0',y1')


    --getPos :: Area -> Point
    --getPos = xy

    --getSize :: Area -> Point
    --getSize = wh

    --b = a >+< a
    --    where
    --        a = Coord {pixel = 12, pos = 1.0}
    --(><) P
    --class Coordinate a where
    --    (>+<) :: a -> a -> a
