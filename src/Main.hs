import Control.Monad (unless, when)
import Control.Monad.Trans.State
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G

import System.Random
import System.Exit
import System.IO
import System.Environment (getArgs)

import Game.Board.Row
import Game.Board
--import Game.World
--bool :: Bool -> a -> a -> a
--bool b falseRes trueRes = if b then trueRes else falseRes
--
--
--unless' :: Monad m => m Bool -> m () -> m ()
--unless' action falseAction = do
--    b <- action
--    unless b falseAction
--
--maybe' :: Maybe a -> b -> (a -> b) -> b
--maybe' m nothingRes f = case m of
--    Nothing -> nothingRes
--    Just x  -> f x
--
---- type ErrorCallback = Error -> String -> IO ()
--errorCallback :: G.ErrorCallback
--errorCallback err description = hPutStrLn stderr description
--
--keyCallback :: G.KeyCallback
--keyCallback window key scancode action mods = when (key == G.Key'Escape && action == G.KeyState'Pressed) $
--  G.setWindowShouldClose window True

main :: IO()
main =
    (mkStdGen <$> read <$> head <$> getArgs) >>= \g->
    print ((evalState (genSolution 3 3) g))
    --let seed =
    --G.setErrorCallback (Just errorCallback)
    --successfulInit <- G.init
    ---- if init failed, we exit the program
    --bool successfulInit exitFailure $ do
    --    mw <- G.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
    --    maybe' mw (G.terminate >> exitFailure) $ \window -> do
    --        G.makeContextCurrent mw
    --        G.setKeyCallback window (Just keyCallback)
    --        loop window
    --        G.destroyWindow window
    --        G.terminate
    --        exitSuccess

--loop :: G.Window -> IO ()
--loop w = unless' (G.windowShouldClose w) $
--       G.getFramebufferSize w >>= \(width, height) ->
--    --let ratio = fromIntegral width / fromIntegral height
--
--       --viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
--       clear [ColorBuffer]
--
--    --matrixMode $= Projection
--    --loadIdentity
--    --ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
--    --matrixMode $= Modelview 0
--
--    >> loadIdentity
--    -- this is bad, but keeps the logic of the original example I guess
--    >> G.getTime >>= print
--    --rotate ((realToFrac t) * 50) $ (Vector3 0 0 1 :: Vector3 GLdouble)
--
--    --renderPrimitive Triangles $ do
--    --    color  (Color3 1 0 0 :: Color3 GLdouble)
--    --    vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
--    --    color  (Color3 0 1 0 :: Color3 GLdouble)
--    --    vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
--    --    color  (Color3 0 0 1 :: Color3 GLdouble)
--    --    vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
--
--    >> G.swapBuffers w
--    >> G.pollEvents
--    >> loop w
