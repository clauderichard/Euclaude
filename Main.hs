module Main
where

import qualified Graphics.UI.GLUT as GL
import Data.IORef
import Control.Monad
import Draw
import Gen
import Shape
import Trihandle


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
  
transInterval :: (Integral i) => i -> i -> R -> R -> i -> R
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
    demo $ trihandleTriangle th
    GL.flush

draw col prop t = drawColouredShapes col [prop t]
    
    
--------------------------------------------------------------------------------
-- Demos
--------------------------------------------------------------------------------
demo :: Triangle -> IO ()
demo = demoOrthocenter


demoMittenpunkt :: Triangle -> IO ()
demoMittenpunkt t = do
    draw colourWhite id t
    draw colourRed (cevians gCentroid) t
    draw colourRed (point gCentroid) t
    draw colourGreen (points gOutcenters) t
    draw colourGreen (circles gOutcenters gOutradii) t
    draw colourCyan (rays gMittenpunkt gOutcenters) t
    draw colourCyan (point gMittenpunkt) t

demoCentroid :: Triangle -> IO ()
demoCentroid t = do
    draw colourWhite id t
    draw colourRed (cevians gCentroid) t
    draw colourRed (point gCentroid) t

demoIncircle t = do
    draw colourWhite id t
    draw colourYellow (rays gIncenter gId) t
    draw colourYellow (point gIncenter) t
    draw colourYellow (circle gIncenter gInradius) t

demoCircumcircle t = do
    draw colourWhite id t
    draw colourCyan (rays gCircumcenter (gCevianIntersects gCentroid)) t
    draw colourGreen (circle gCircumcenter gCircumradius) t

demoOrthocenter t = do
    draw colourRed (lineSegments (gCevianIntersects gOrthocenter) (gCevianIntersects gCentroid)) t
    draw colourWhite id t
    draw colourCyan (cevians gOrthocenter) t
    draw colourCyan (rays gOrthocenter gId) t
    draw colourCyan (point gOrthocenter) t

demoSymmedian t = do
    draw colourWhite id t
    draw colourCyan (cevians gSymmedian) t
    draw colourCyan (point gSymmedian) t

demoNagel t = do
    draw colourWhite id t
    draw colourCyan (cevians gNagel) t
    draw colourCyan (point gNagel) t
    
demoEulerLine t = do
    draw colourRed (lineSegments (gCevianIntersects gOrthocenter) (gCevianIntersects gCentroid)) t
    draw colourWhite id t
    draw colourCyan (cevians gOrthocenter) t
    draw colourCyan (rays gOrthocenter gId) t
    draw colourCyan (point gOrthocenter) t
    draw colourGreen (cevians gCentroid) t
    draw colourGreen (point gCentroid) t
    draw colourBlue (point gCircumcenter) t
    draw colourBlue (circle gCircumcenter gCircumradius) t
    draw colourMagenta (point gExeter) t
    draw colourYellow (lineSegment gOrthocenter gExeter) t
    draw colourYellow (lineSegment gCircumcenter gExeter) t
    draw colourYellow (point gNinePointCenter) t
    draw colourYellow (circle gNinePointCenter gNinePointRadius) t

demoNinePointCircle t = do
    draw colourRed (lineSegments (gCevianIntersects gOrthocenter) (gCevianIntersects gCentroid)) t
    draw colourWhite id t
    draw colourCyan (cevians gOrthocenter) t
    draw colourCyan (rays gOrthocenter gId) t
    draw colourCyan (points (gCevianIntersects gOrthocenter)) t
    draw colourGreen (points (gCevianIntersects gCentroid)) t
    draw colourBlue (raysMidpoints gOrthocenter gId) t
    draw colourYellow (point gNinePointCenter) t
    draw colourYellow (circle gNinePointCenter gNinePointRadius) t
