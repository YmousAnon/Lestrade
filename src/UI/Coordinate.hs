module UI.Coordinate
(
    Coord,


    Point,
    (>+<),
    (>-<),
    (>*<),
    (>/<),

    getX,
    getY,

    pointToGL,
    posToPoint,
    pointInArea,


    Movable,
    moveTo,
    moveBy,

    Area (Area, Empty),
    (\/),
    newArea,

    getAreaStart,
    getAreaSize,
    getAreaEnd,

    getXRange,
    getYRange,

    getWidth,
    getHeight,

    xRangeToGL,
    yRangeToGL,

    getXMin,
    getXMax,
    getYMin,
    getYMax,

    inArea,
) where

    import Graphics.Rendering.OpenGL

    import UI.Input.Settings


    type Coord = Int

    xCoordToGL :: Area -> Coord -> GLfloat
    xCoordToGL window x = -1+2*(fromIntegral x/fromIntegral (getXMax window))

    yCoordToGL :: Area -> Coord -> GLfloat
    yCoordToGL window y =  1-2*(fromIntegral y/fromIntegral (getYMax window))



    type Point = (Coord,Coord)

    (>+<) :: Point -> Point -> Point
    (x,y) >+< (x',y') = (x+x',y+y')

    (>-<) :: Point -> Point -> Point
    (x,y) >-< (x',y') = (x-x',y-y')

    (>*<) :: Point -> Point -> Point
    (x,y) >*< (x',y') = (x*x',y*y')

    (>/<) :: Point -> Point -> Point
    (x,y) >/< (x',y') = (div x x',div y y')

    getX :: Point -> Coord
    getX (x,y) = x

    getY :: Point -> Coord
    getY (x,y) = y

    pointToGL :: Area -> Point -> (GLfloat,GLfloat)
    pointToGL window (x,y) = (xCoordToGL window x,yCoordToGL window y)

    posToPoint :: (Double,Double) -> Point
    posToPoint (x,y) = (round x,round y)

    pointInArea :: Point -> Area -> Bool
    pointInArea (x,y) a = x > getXMin a && x < getXMax a
                       && y > getYMin a && y < getYMax a




    class Movable a where
        moveTo :: Point -> a -> a
        moveBy :: Point -> a -> a

    data Area = Empty | Area
        { xy :: Point
        , wh :: Point
        }

    instance Show Area where
        show a = show (x,y)++" - "++show (x+w,y+h)
            where (x,y) = xy a
                  (w,h) = wh a

    instance Movable Area where
        moveTo xy' Area { xy = xy      , wh = wh } =
                   Area { xy = xy'     , wh = wh }
        moveBy dxy Area { xy = xy      , wh = wh } =
                   Area { xy = xy>+<dxy, wh = wh }

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



    newArea :: Point -> Coord -> Coord -> Area
    newArea xy w h = Area { xy = xy, wh = (w,h) }



    getAreaStart :: Area -> Point
    getAreaStart Area { xy = xy } = xy

    getAreaSize :: Area -> Point
    getAreaSize Area { wh = wh } = wh

    getAreaEnd :: Area -> Point
    getAreaEnd Area { xy = xy, wh = wh } = xy>+<wh



    getXRange :: Area -> Point
    getXRange Area { xy = (x,_), wh = (w,_) } = (x,x+w)

    getYRange :: Area -> Point
    getYRange Area { xy = (_,y), wh = (_,h) } = (y,y+h)



    getWidth :: Area -> Coord
    getWidth Area { wh = (w,_) } = w

    getHeight :: Area -> Coord
    getHeight Area { wh = (_,h) } = h



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



    inArea :: Point -> Area -> Bool
    inArea pt a = fst pt >= getXMin a
               && fst pt <= getXMax a
               && snd pt >= getYMin a
               && snd pt <= getYMax a
