module UI
(
    UI(UI),
    window,
    dirty,
    time,

    unlessClose,
    resizeWindow,
    swapBuffers',

    cleanWindow,
    dirtyWindow,
    writeDirty,
    whenDirty,

    fpsWait,

    playMainClick,
    playSecondaryClick,
) where

    import Control.Concurrent
    import Control.Monad

    import Data.IORef
    import Data.Maybe

    import UI.Coordinate
    import UI.Input.Settings

    import Graphics.UI.GLFW


    data UI = UI
              { window         :: Window
              , dirty          :: IORef Bool
              , time           :: IORef Double
              , mainClick      :: IO()
              , secondaryClick :: IORef [IO()]
              }



    unlessClose :: UI -> IO() -> IO()
    unlessClose ui action = do
        close <- windowShouldClose (window ui)
        unless close action

    resizeWindow :: UI -> Area -> IO()
    resizeWindow ui area =
           setWindowSize (window ui) (fromIntegral $ getXMax area)
                                     (fromIntegral $ getYMax area)

    swapBuffers' :: UI -> IO()
    swapBuffers' = swapBuffers . window



    cleanWindow :: UI -> IO()
    cleanWindow ui = writeIORef (dirty ui) False

    dirtyWindow :: UI -> IO()
    dirtyWindow ui = writeIORef (dirty ui) True

    writeDirty :: IORef Bool -> WindowRefreshCallback
    writeDirty dirty window = writeIORef dirty True

    whenDirty :: UI -> IO() -> IO()
    whenDirty ui action = do dirty <- readIORef $ dirty ui

                             when dirty action



    fpsWait :: UI -> IO() -> IO()
    fpsWait ui action = do
        t_last    <- readIORef $ time ui
        t_current <- fromJust <$> getTime
        fps       <- read <$> getSetting "fps"

        threadDelay $ round (1000000*(1/fps -( t_current - t_last)))
        getTime >>= writeIORef (time ui) . fromJust

        action



    playMainClick :: UI -> IO()
    playMainClick = mainClick

    playSecondaryClick :: UI -> IO()
    playSecondaryClick ui =
        let ioRCLs = secondaryClick ui
         in readIORef ioRCLs >>= \(cl:cls) -> cl >> writeIORef ioRCLs cls
