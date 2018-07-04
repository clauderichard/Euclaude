--------------------------------------------------------------------------------
-- The main program is here, which creates an OpenGL window
-- that displays a triangle with some of its special properties.
-- The triangle's corners can be dragged and dropped with the mouse.
--------------------------------------------------------------------------------

module Main
where

import qualified Graphics.UI.GLUT as GL
import Data.IORef
import Control.Monad

import Demos
import Trihandle
import Shape


windowTitle :: String
windowTitle = "Euclaude"

windowWidth = 300
windowHeight = 300
 
main :: IO ()
main = do
  (_progName, _args) <- GL.getArgsAndInitialize
  _window <- GL.createWindow windowTitle
  phandle <- newIORef makePolyhandle5
  GL.keyboardMouseCallback GL.$= Just (keyboardMouse phandle)
  GL.displayCallback GL.$= display phandle
  GL.reshapeCallback GL.$= Just reshape
  GL.motionCallback GL.$= Just (mouseMotion phandle)
  GL.passiveMotionCallback GL.$= Just (mouseMotion phandle)
  reshape (GL.Size windowWidth windowHeight)
  GL.mainLoop
  
transInterval :: (Integral i, Floating r) => i -> i -> r -> r -> i -> r
transInterval oldMin oldMax newMin newMax oldX = numer / denom
  where numer = newMin * fromIntegral (oldMax-oldX) + newMax * fromIntegral (oldX-oldMin)
        denom = fromIntegral $ oldMax - oldMin

posFromGL :: GL.Position -> Point
posFromGL (GL.Position a b) = Point a' b'
  where oldMinW = 0
        oldMaxW = windowWidth
        oldMinH = 0
        oldMaxH = windowHeight
        newMin = -1
        newMax = 1
        a' = transInterval oldMinW oldMaxW newMin newMax $ a
        b' = transInterval oldMinH oldMaxH newMax newMin $ b
  
keyboardMouse :: IORef Polyhandle -> GL.KeyboardMouseCallback
keyboardMouse phandle key GL.Down _ position = do
    phandle GL.$~! polyhandleDown (posFromGL position)
    display phandle
keyboardMouse phandle key GL.Up _ position = do
    phandle GL.$~! polyhandleUp (posFromGL position)
    display phandle

mouseMotion :: IORef Polyhandle -> GL.MotionCallback
mouseMotion phandle position = do
    phandle GL.$~! polyhandleMove (posFromGL position)
    display phandle

reshape :: GL.ReshapeCallback
reshape size = do
  GL.viewport GL.$= (GL.Position 0 0, size)
  GL.postRedisplay Nothing
 
display :: IORef Polyhandle -> GL.DisplayCallback
display phandle = do
    GL.clear [GL.ColorBuffer]
    ph <- readIORef phandle
    mainDemo $ polyhandleTriangle ph
    --mainPentagonDemo $ polyhandlePentagon ph
    GL.flush

