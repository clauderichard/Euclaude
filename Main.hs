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

 
main :: IO ()
main = do
  (_progName, _args) <- GL.getArgsAndInitialize
  _window <- GL.createWindow windowTitle
  thandle <- newIORef makeTrihandle
  GL.keyboardMouseCallback GL.$= Just (keyboardMouse thandle)
  GL.displayCallback GL.$= display thandle
  GL.reshapeCallback GL.$= Just reshape
  GL.motionCallback GL.$= Just (mouseMotion thandle)
  GL.passiveMotionCallback GL.$= Just (mouseMotion thandle)
  reshape (GL.Size 300 300)
  GL.mainLoop
  
transInterval :: (Integral i, Floating r) => i -> i -> r -> r -> i -> r
transInterval oldMin oldMax newMin newMax oldX = numer / denom
  where numer = newMin * fromIntegral (oldMax-oldX) + newMax * fromIntegral (oldX-oldMin)
        denom = fromIntegral $ oldMax - oldMin

posFromGL :: GL.Position -> Point
posFromGL (GL.Position a b) = Point a' b'
  where oldMin = 0
        oldMax = 300
        newMin = -1
        newMax = 1
        a' = transInterval oldMin oldMax newMin newMax $ a
        b' = transInterval oldMin oldMax newMax newMin $ b
  
keyboardMouse :: IORef Trihandle -> GL.KeyboardMouseCallback
keyboardMouse thandle key GL.Down _ position = do
    thandle GL.$~! trihandleDown (posFromGL position)
    display thandle
keyboardMouse thandle key GL.Up _ position = do
    thandle GL.$~! trihandleUp (posFromGL position)
    display thandle

mouseMotion :: IORef Trihandle -> GL.MotionCallback
mouseMotion thandle position = do
    thandle GL.$~! trihandleMove (posFromGL position)
    display thandle

reshape :: GL.ReshapeCallback
reshape size = do
  GL.viewport GL.$= (GL.Position 0 0, size)
  GL.postRedisplay Nothing
 
display :: IORef Trihandle -> GL.DisplayCallback
display thandle = do
    GL.clear [GL.ColorBuffer]
    th <- readIORef thandle
    mainDemo $ trihandleTriangle th
    GL.flush

