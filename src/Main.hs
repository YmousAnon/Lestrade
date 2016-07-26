import Control.Monad
import Data.IORef

import Game

import Graphics.UI.GLFW

import Interface
import Interface.Input
import Interface.Input.Seed
import Interface.Render


--import Data.StateVar
--import Data.Maybe
--import Game.Board.Value

--v :: IO Value
--v = value 1 True (100,100) 1


main :: IO()
main = getSeed >>= gameInit >>= guiInit >>= uncurry (loop mouseKeysUp)
    where
        loop :: (IORef Bool -> Window -> Game -> IO (Action Game)) -> Game ->
                Screen -> IO()
        loop action game s = windowShouldClose (window s) >>= \close ->
            unless close $ do
                --getTime >>= print

                display (window s) (dirty s) game

                Action (game',action') <- action (dirty s) (window s) game

                loop action' game' s
