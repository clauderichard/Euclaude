module Main
where

import qualified Graphics.UI.GLUT as GL
import Data.IORef
import Control.Monad
import Draw
import Gen
import Shape
import Animator


windowTitle :: String
windowTitle = "Euclaude"

 
main :: IO ()
main = do
  (_progName, _args) <- GL.getArgsAndInitialize
  _window <- GL.createWindow windowTitle
  anim <- newIORef makeTriangleAnimator
  delta <- newIORef (0.02::R)
  pos <- newIORef (0::R, 0::R)
  GL.keyboardMouseCallback GL.$= Just (keyboardMouse delta pos)
  GL.idleCallback GL.$= Just (idle delta anim)
  GL.displayCallback GL.$= display anim pos
  GL.reshapeCallback GL.$= Just reshape
  GL.mainLoop
  
keyboardMouse :: IORef R -> IORef (R, R) -> GL.KeyboardMouseCallback
keyboardMouse a p key GL.Down _ _ = case key of
  (GL.Char ' ') -> a GL.$~! negate
  (GL.Char '+') -> a GL.$~! (* 2)
  (GL.Char '-') -> a GL.$~! (/ 2)
  (GL.SpecialKey GL.KeyLeft ) -> p GL.$~! \(x,y) -> (x-0.1,y)
  (GL.SpecialKey GL.KeyRight) -> p GL.$~! \(x,y) -> (x+0.1,y)
  (GL.SpecialKey GL.KeyUp   ) -> p GL.$~! \(x,y) -> (x,y+0.1)
  (GL.SpecialKey GL.KeyDown ) -> p GL.$~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()
  
idle :: IORef R -> IORef TriangleAnimator -> GL.IdleCallback
idle delta anim = do
  --anim GL.$~! (id)
  d <- GL.get delta
  anim GL.$~! (stepTriangleAnimator d)
  GL.postRedisplay Nothing
 
reshape :: GL.ReshapeCallback
reshape size = do
  GL.viewport GL.$= (GL.Position 0 0, size)
  GL.postRedisplay Nothing
 
display :: IORef TriangleAnimator -> IORef (R,R) -> GL.DisplayCallback
display anim pos = do
    GL.clear [GL.ColorBuffer]
    a <- readIORef anim
    p <- readIORef pos
    demo $ animatedTriangle $ moveTriangleAnimator p a
    GL.flush

draw col prop t = drawColouredShapes col [prop t]
    
    
--------------------------------------------------------------------------------
-- Demos
--------------------------------------------------------------------------------
demo :: Triangle -> IO ()
demo = demoEulerLine


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
