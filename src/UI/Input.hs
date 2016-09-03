module UI.Input
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

    import UI
    import UI.Coordinate
    import UI.Render


    class Clickable a where
        lclick :: UI -> Point -> a -> IO a
        rclick :: UI -> Point -> a -> IO a



    data Action a = Action (a,UI -> a -> IO (Action a))



    mouseKeysUp :: (Renderable a,Clickable a) => UI -> a -> IO(Action a)
    mouseKeysUp ui game = do
        (lButton,rButton,pt) <- getInput (window ui) game

        let ioGame'    | lButton            = lclick ui pt game
                       | rButton            = rclick ui pt game
                       | otherwise          = return       game
            nextAction | lButton || rButton = mouseKeyDown
                       | otherwise          = mouseKeysUp
            writeDirty | lButton || rButton = dirtyWindow ui
                       | otherwise          = return()

         in do writeDirty
               game' <- ioGame'
               return $ Action (game', nextAction)

    mouseKeyDown :: (Renderable a,Clickable a) => UI -> a -> IO(Action a)
    mouseKeyDown ui game = do
        (lButton,rButton,_) <- getInput (window ui) game

        let nextAction | lButton || rButton = mouseKeyDown
                       | otherwise          = mouseKeysUp
         in return $ Action (game, nextAction)

    getInput :: Renderable a => Window -> a -> IO(Bool,Bool,Point)
    getInput w game = do
        pollEvents

        l  <- (==MouseButtonState'Pressed) <$> getMouseButton w MouseButton'1
        r  <- (==MouseButtonState'Pressed) <$> getMouseButton w MouseButton'2

        pt <- getCursorPoint w game

        return (l,r,pt)

    getCursorPoint :: Renderable a => Window -> a -> IO Point
    getCursorPoint w game = do
        (_,yM) <- getWindowSize w
        (x,y ) <- getCursorPos  w
        let dy =  yM-(getYMax $ getArea game)
        return (round x,round y-dy)
