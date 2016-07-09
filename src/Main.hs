import Game

import Graphics.UI.GLUT

import Interface
import Interface.Input.Seed


main :: IO()
main = getSeed >>= gameInit >>= guiInit >> mainLoop
