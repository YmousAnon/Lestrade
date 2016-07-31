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
    import Interface.Screen


    class Clickable a where
        lclick :: Point -> a -> IO a
        rclick :: Point -> a -> IO a



    data Action a = Action (a,Screen -> a -> IO (Action a))



    mouseKeysUp :: Clickable a => Screen -> a -> IO(Action a)
    mouseKeysUp screen game = do
        (lButton,rButton,pt) <- getInput $ window screen

        let ioGame'    | lButton            = lclick pt game
                       | rButton            = rclick pt game
                       | otherwise          = return    game
            nextAction | lButton || rButton = mouseKeyDown
                       | otherwise          = mouseKeysUp
            writeDirty | lButton || rButton = writeIORef (dirty screen) True
                       | otherwise          = return()

         in do writeDirty
               game' <- ioGame'
               return $ Action (game', nextAction)

    mouseKeyDown :: Clickable a => Screen -> a -> IO(Action a)
    mouseKeyDown screen game = do
        (lButton,rButton,_) <- getInput $ window screen

        let nextAction | lButton || rButton = mouseKeyDown
                       | otherwise          = mouseKeysUp
         in return $ Action (game, nextAction)

    getInput :: Window -> IO (Bool,Bool,Point)
    getInput w = do
        pollEvents

        l  <- (==MouseButtonState'Pressed) <$> getMouseButton w MouseButton'1
        r  <- (==MouseButtonState'Pressed) <$> getMouseButton w MouseButton'2

        pt <- posToPoint <$> getCursorPos w

        return (l,r,pt)
