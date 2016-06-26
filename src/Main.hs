import Control.Monad (unless, when)
import Control.Monad.Trans.State

import Data.Maybe

--import Graphics.GLUtil
--import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT (mainLoop)
--import Graphics.UI.GLFW

import System.Random
import System.Exit
import System.IO
import System.Environment (getArgs)

--import Settings
--import Game
--
import Interface
--import Interface.Texture
import Data.IORef

import Interface.Coordinate
--import Game
--0import Game.Board
import Game.Board.Row
import Game.Board.Square
import Game.Board.Value

--import Settings

v :: IO Value
v = value 1 True (0,0) 3
s :: IO Square
s = solvedSquare 1 1 7 (100,100)
r :: IO Row
r = newRow 1 7 (0,0)
--s = unsolvedSquare [1..5] 1 5 (0,0)
--s :: Square
--s = square [1] 1 0 8 0.2 (-1,-1)
--s = square [1,3,4,6] 0 8 0.1 (-1,-1)
--s :: Square
--s = square [1..8] 1 8 0.1 (-1,-1)
--r1 = newRow 1 8 0.1 (-1,-1)
--as = replicate 8 $ newRow 1 8 0.1 (0,0)
--a = newBoard 2 8


main :: IO()
main = r >>= newIORef >>= guiInit >> mainLoop
--main = return()--guiInit >>= loop 0
--main = (newIORef =<< unsolvedSquare [0,1,2,3] 1 8 (200,100)) >>= guiInit >> mainLoop-- = loop 0
--main = (newIORef =<< unsolvedSquare [0,1,2,3] 1 8 (200,100)) >>= guiInit >> mainLoop-- = loop 0
--main = newIORef (newBoard 2 8 0.1 (-1,-1)) >>= guiInit >> mainLoop-- = loop 0
--main = newIORef (newBoard 2 8 0.1 (-1,-1)) >>= guiInit >> mainLoop-- = loop 0
    --where
    --    loop :: Int -> Window -> IO()
    --    loop i w = do
    --    --loop i w = windowShouldClose w >>= \close -> unless close $ do

    --        GLFW.getTime >>= (print . fromJust)
    --        --clear [ColorBuffer, DepthBuffer]
    --        --render r
    --        --draw s
    --        --draw r1
    --        --draw v
    --        swapBuffers w
    --        --pollEvents
    --        loop (i+1) w
    --initWindow screenRes "Sherlock"
    --initGraphics screenResWidth screenResHeight

--loop :: TextureObject -> GLFW.Window -> IO ()
--loop tex w = GLFW.windowShouldClose w >>= \close -> unless close $
    --where
    --    screenRes = Size (truncate screenResWidth) (truncate screenResHeight)

    --    screenResWidth :: GLdouble
    --    screenResWidth = 1024.0

    --    screenResHeight :: GLdouble
    --    screenResHeight = 768.0

--tex32 :: IO TextureObject
--tex32 = loadTextureFromFile "tex32.png"
--tex16 :: IO TextureObject
--tex16 = loadTextureFromFile "tex16.png"
--tex = loadGLTexture "res/Images/1.png"




--import Game.World
--bool :: Bool -> a -> a -> a
--bool b falseRes trueRes = if b then trueRes else falseRes


--unless' :: Monad m => m Bool -> m () -> m ()
--unless' action falseAction = do
--    b <- action
--    unless b falseAction
--
--maybe' :: Maybe a -> b -> (a -> b) -> b
--maybe' m nothingRes f = case m of
--    Nothing -> nothingRes
--    Just x  -> f x
----
------ type ErrorCallback = Error -> String -> IO ()
--
--keyCallback :: GLFW.KeyCallback
--keyCallback window key scancode action mods = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
--  GLFW.setWindowShouldClose window True

--bm = (readBitmap "res/Images/1.bmp") >>= makeSimpleBitmapTexture

--main :: IO()
--main =
--    --(mkStdGen <$> read <$> head <$> getArgs) >>= \g->
--
--    --read <$> getVal "columns" >>= \nC ->
--    --read <$> getVal "rows"    >>= \nR ->
--
--    ----let (b,g') = runState (genSolvedBoard nR nC) g
--    --let (game,g') = newGame nR nC g--runState (genSolvedBoard nR nC) g
--
--    --in guiInit >>= preMainLoop --loop --preMainLoop--loadGLTexture "res/Images/1.png" >>= \tex ->
--       loadGLTexture "res/Images/1.png" >>= \tex ->
--       guiInit                          >>= \w ->
--       loop tex w -- \w -> preMainLoop w -- >> loop w
--    --print game >>
--    --let seed =
--    -- if init failed, we exit the program
--
--    --bool successfulInit exitFailure $ do
--    --    mw <- GLFW.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
--    --    maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
--    --        GLFW.makeContextCurrent mw
--    --        GLFW.setKeyCallback window (Just keyCallback)
--    --        loop window
--    --        GLFW.destroyWindow window
--    --        GLFW.terminate
--    --        exitSuccess
--preMainLoop :: GLFW.Window -> IO ()
--preMainLoop window = do
--    tex <- loadGLTexture "res/Images/1.png"
--    --clearColor $= Color4 0.9 0.1243 0.2544564 1.0
--    --depthFunc $= Just Lequal
--    --blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
--    --normalize $= Enabled
--    --texture Texture2D $= Enabled
--    --shadeModel $= Smooth
--    loop tex window
--
--
--
--
--mainLoop :: TextureObject -> Window -> IO ()
--mainLoop tex window = do
--    action <- (windowShouldClose window)
--    unless action $ do
--        --viewWindow window
--        cal tex
--        swapBuffers window
--        pollEvents
--        mainLoop tex window
--
--cal tex = do
--    preservingMatrix $ do
--        rotate 90 (Vector3 1 0 0 :: Vector3 GLfloat)
--        withTextures2D [tex] $ draw tex
--
--loop :: TextureObject -> Window -> IO ()
--loop tex w = windowShouldClose w >>= \close -> unless close $
--
--       cal tex
--    >> GLFW.getTime >>= (print . fromJust)
--    >> swapBuffers w
--    >> pollEvents
--    >> loop tex w
--loop :: TextureObject -> GLFW.Window -> IO ()
--loop tex w = GLFW.windowShouldClose w >>= \close -> unless close $
--       --GLFW.getFramebufferSize w >>= \(width, height) ->
--    --let ratio = fromIntegral width / fromIntegral height
--
--    --viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
--    viewWindow w
--    >> draw tex
--
--    --matrixMode $= Projection
--    --loadIdentity
--    --ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
--    --matrixMode $= Modelview 0
--
--    >> GL.loadIdentity
--    -- this is bad, but keeps the logic of the original example I guess
--    >> GLFW.getTime >>= (print . fromJust)
--    >> (readTexture "res/Images/1.png" :: IO (Either String GL.TextureObject))
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
--    >> GLFW.swapBuffers w
--    >> GLFW.pollEvents
--    >> loop tex w

