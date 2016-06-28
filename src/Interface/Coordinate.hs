module Interface.Coordinate
(
    Coord,

    Point,

    getX,
    getY,

    pointToGL,



    Area (Area, Empty),
    newArea,

    getXRange,
    getYRange,

    xRangeToGL,
    yRangeToGL,

    getXMin,
    getXMax,
    getYMin,
    getYMax,

    (\/)
) where

    import Graphics.UI.GLUT hiding (Point)

    import Settings

    import Unsafe.Coerce

    type Coord = Int


    xCoordToGL :: Area -> Coord -> GLfloat
    xCoordToGL window x = 2*(fromIntegral x/fromIntegral (getXMax window))-1

    yCoordToGL :: Area -> Coord -> GLfloat
    yCoordToGL window y = 2*(fromIntegral y/fromIntegral (getYMax window))-1


    type Point = (Coord,Coord)

    getX :: Point -> Coord
    getX (x,y) = x

    getY :: Point -> Coord
    getY (x,y) = y

    pointToGL :: Area -> Point -> (GLfloat,GLfloat)
    pointToGL window (x,y) = (xCoordToGL window x,yCoordToGL window y)


    data Area = Empty | Area
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


    xRangeToGL :: Area -> Point -> (GLfloat,GLfloat)
    xRangeToGL window (x,x') = (xCoordToGL window x, xCoordToGL window x')

    yRangeToGL :: Area -> Point -> (GLfloat,GLfloat)
    yRangeToGL window (y,y') = (yCoordToGL window y, yCoordToGL window y')


    getXMin :: Area -> Coord
    getXMin a = fst $ getXRange a

    getXMax :: Area -> Coord
    getXMax a = snd $ getXRange a

    getYMin :: Area -> Coord
    getYMin a = fst $ getYRange a

    getYMax :: Area -> Coord
    getYMax a = snd $ getYRange a

    (\/) :: Area -> Area -> Area
    Empty \/ a     = a
    a     \/ Empty = a
    a     \/ a'    = Area
        { xy = (x0   ,y0   )
        , wh = (x1-x0,y1-y0)
        }
        where
            x0 = min (getXMin a) (getXMin a')
            x1 = max (getXMax a) (getXMax a')
            y0 = min (getYMin a) (getYMin a')
            y1 = max (getYMax a) (getYMax a')
