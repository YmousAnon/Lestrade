module Interface.Screen
(
    Screen (Screen),
    window,
    dirty,
    time,

    unlessScreenShouldClose,
    resizeScreen,
    swapBuffers',

    writeDirty,
    cleanScreen,
    whenDirty,

    fpsWait,
) where

    import Control.Concurrent
    import Control.Monad

    import Data.IORef
    import Data.Maybe

    import Interface.Coordinate
    import Interface.Input.Settings

    import Graphics.UI.GLFW


    data Screen = Screen
                  { window :: Window
                  , dirty  :: IORef Bool
                  , time   :: IORef Double
                  }



    unlessScreenShouldClose :: Screen -> IO() -> IO()
    unlessScreenShouldClose s action = do
        close <- windowShouldClose (window s)
        unless close action

    resizeScreen :: Screen -> Area -> IO()
    resizeScreen s a =
           setWindowSize (window s) (fromIntegral $ getXMax $ a)
                                    (fromIntegral $ getYMax $ a)

    swapBuffers' :: Screen -> IO()
    swapBuffers' = swapBuffers . window



    writeDirty :: IORef Bool -> WindowRefreshCallback
    writeDirty dirty w = writeIORef dirty True

    cleanScreen :: Screen -> IO()
    cleanScreen s = writeIORef (dirty s) False

    whenDirty :: Screen -> IO() -> IO()
    whenDirty s action = do dirty <- readIORef $ dirty s

                            when dirty action



    fpsWait :: Screen -> IO() -> IO()
    fpsWait s action = do
        t_last    <- readIORef $ time s
        t_current <- fromJust <$> getTime
        fps       <- read <$> getSetting "fps"

        threadDelay $ round (1000000*(1/fps -( t_current - t_last)))
        getTime >>= writeIORef (time s) . fromJust

        action
