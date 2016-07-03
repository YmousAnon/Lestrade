import Game

import Graphics.UI.GLUT

import Interface


main :: IO()
main = gameInit >>= guiInit >> mainLoop
