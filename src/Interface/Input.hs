module Interface.Input
(
    Clickable,
    lclick,
    rclick,

    Action (Action),
    mouseKeysUp,
) where

    import Control.Monad

    import Data.IORef
    import Data.Time

    import Graphics.GLUtil
    import Graphics.UI.GLFW

    import Interface.Coordinate
    import Interface.Render


    class Clickable a where
        lclick :: Point -> a -> IO a
        rclick :: Point -> a -> IO a



    data Action a = Action (a,IORef Bool -> Window -> a -> IO (Action a))



    mouseKeysUp :: Clickable a => IORef Bool -> Window -> a -> IO(Action a)
    mouseKeysUp dirty w game = do
        pollEvents

        lButton <- lPress w
        rButton <- rPress w

        pt      <- posToPoint <$> getCursorPos w

        let ioGame'    | lButton            = lclick pt game
                       | rButton            = rclick pt game
                       | otherwise          = return    game
            nextAction | lButton || rButton = mouseKeyDown
                       | otherwise          = mouseKeysUp
            writeDirty | lButton || rButton = writeIORef dirty True
                       | otherwise          = return()

         in do writeDirty
               game' <- ioGame'
               return $ Action (game', nextAction)

    mouseKeyDown :: Clickable a => IORef Bool -> Window -> a -> IO(Action a)
    mouseKeyDown _ w game = do
        pollEvents

        lButton <- lPress w
        rButton <- rPress w

        let nextAction | lButton || rButton = mouseKeyDown
                       | otherwise          = mouseKeysUp
         in return $ Action (game, nextAction)



    lPress :: Window -> IO Bool
    lPress w = (==MouseButtonState'Pressed) <$> getMouseButton w MouseButton'1

    rPress :: Window -> IO Bool
    rPress w = (==MouseButtonState'Pressed) <$> getMouseButton w MouseButton'2
