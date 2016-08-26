import Data.IORef

import Game

import Graphics.UI.GLFW

import UI
import UI.Init
import UI.Input
import UI.Input.Seed
import UI.Render


main :: IO()
main = getSeed >>= gameInit >>= uiInit (loop mouseKeysUp)
    where
        loop :: (UI -> Game -> IO(Action Game)) -> Game -> UI -> IO()
        loop action game ui = unlessClose ui $ fpsWait ui $ do

            display ui game

            Action (game',action') <- action ui game

            game'' <- gameTimeStep ui game'

            loop action' game'' ui
