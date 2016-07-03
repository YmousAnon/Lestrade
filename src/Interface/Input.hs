module Interface.Input
(
    Clickable,
    lclick,
    rclick,

    click,
) where

    import Control.Monad

    import Data.IORef
    import Data.Time

    import Graphics.GLUtil
    import Graphics.UI.GLUT hiding (GLfloat)

    import Interface.Coordinate
    import Interface.Render


    class Clickable a where
        lclick :: Point -> a -> IO a
        rclick :: Point -> a -> IO a


    click :: (Renderable a, Clickable a) => IORef a -> KeyboardMouseCallback
    click  ioGame _key _state _modifiers _position = do
        game <- readIORef ioGame

        unless (_state == Up)
            (writeIORef ioGame =<<
                    (case _key of
                        MouseButton LeftButton  -> lclick (posToPt _position)
                        MouseButton RightButton -> rclick (posToPt _position)
                        _                       -> return) game)

        display ioGame
