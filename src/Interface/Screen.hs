module Interface.Screen
(
    Screen (Screen),
    window,
    dirty,
    time,

    unlessClose,
    resizeScreen,
    swapBuffers',

    cleanScreen,
    dirtyScreen,
    writeDirty,
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



    unlessClose :: Screen -> IO() -> IO()
    unlessClose screen action = do
        close <- windowShouldClose (window screen)
        unless close action

    resizeScreen :: Screen -> Area -> IO()
    resizeScreen screen area =
           setWindowSize (window screen) (fromIntegral $ getXMax area)
                                         (fromIntegral $ getYMax area)

    swapBuffers' :: Screen -> IO()
    swapBuffers' = swapBuffers . window



    cleanScreen :: Screen -> IO()
    cleanScreen screen = writeIORef (dirty screen) False

    dirtyScreen :: Screen -> IO()
    dirtyScreen screen = writeIORef (dirty screen) True

    writeDirty :: IORef Bool -> WindowRefreshCallback
    writeDirty dirty window = writeIORef dirty True

    whenDirty :: Screen -> IO() -> IO()
    whenDirty screen action = do dirty <- readIORef $ dirty screen

                                 when dirty action



    fpsWait :: Screen -> IO() -> IO()
    fpsWait screen action = do
        t_last    <- readIORef $ time screen
        t_current <- fromJust <$> getTime
        fps       <- read <$> getSetting "fps"

        threadDelay $ round (1000000*(1/fps -( t_current - t_last)))
        getTime >>= writeIORef (time screen) . fromJust

        action
