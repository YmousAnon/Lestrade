import Data.IORef

import Game

import Graphics.UI.GLFW

import Interface
import Interface.Input
import Interface.Input.Seed
import Interface.Render
import Interface.Screen


main :: IO()
main = getSeed >>= gameInit >>= guiInit >>= uncurry (loop mouseKeysUp)
    where
        loop :: (Screen -> Game -> IO(Action Game)) -> Game -> Screen -> IO()
        loop action game screen = unlessClose screen $ fpsWait screen $ do

            display screen game

            Action (game',action') <- action screen game

            game'' <- gameTimeStep screen game'

            loop action' game'' screen
