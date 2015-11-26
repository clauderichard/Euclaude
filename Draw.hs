--------------------------------------------------------------------------------
-- Contains the draw function, which takes in a colour, and
-- a point, line, polygon or circle (or a list of them),
-- and draws it with OpenGL.
--------------------------------------------------------------------------------

module Draw
where

import qualified Graphics.UI.GLUT as GL
import Shape
import Geo

-- Shapes

class Shape a where
  drawShape :: a -> IO ()
  
instance Shape a => Shape [a] where
  drawShape shapes = mapM_ drawShape shapes
instance Shape Point where
  drawShape l = GL.renderPrimitive GL.Polygon $ mapM_ setPoint $ getPointDrawCorners l
instance Shape Line where
  drawShape l = GL.renderPrimitive GL.Lines $ mapM_ setPoint $ linePoints l
instance Shape Triangle where
  drawShape = drawPolygon
instance Shape Tetragon where
  drawShape = drawPolygon
instance Shape Pentagon where
  drawShape = drawPolygon
instance Shape Circle where
  drawShape c = GL.renderPrimitive GL.LineLoop $ mapM_ setPoint $ getCircleDrawCorners c
  
  
  

cyclePolygon :: Polygon a => Int -> a -> a
cyclePolygon i a = fromCorners $ drop i $ cycle $ corners a

drawPolygon :: Polygon a => a -> IO ()
drawPolygon a = GL.renderPrimitive GL.LineLoop $ mapM_ setPoint $ corners a



  
getPointDrawCorners :: Point -> [Point]
getPointDrawCorners (Point x y) =
  let d = 0.015
      e = d * sqrt 2
      x1 = x - d
      y1 = y - d
      x2 = x + d
      y2 = y + d
      x_1 = x - e
      x_2 = x + e
      y_1 = y - e
      y_2 = y + e
  in [Point x1 y1, Point x y_1, 
      Point x2 y1, Point x_2 y,
      Point x2 y2, Point x y_2,
      Point x1 y2, Point x_1 y]
      
getCircleDrawCorners :: Circle -> [Point]
getCircleDrawCorners (Circle (Point x y) r) =
  let n = 50
      angles = map (\i -> 2*pi*i/n) [1..n]
  in map (\a -> Point (x + r * sin a) (y + r * cos a)) angles

--type TriangleFeature = Triangle -> [Shape]


rToGLfloat :: R -> GL.GLfloat
rToGLfloat = realToFrac

colourEmpty :: R
colourEmpty = 0.0
colourFull :: R
colourFull = 1.0
colourPrime :: R -> R
colourPrime r = r * 0.6

faded :: Colour -> Colour
faded (Colour r g b) = Colour (colourPrime r) (colourPrime g) (colourPrime b)

data Colour = Colour R R R
black :: Colour
black = Colour colourEmpty colourEmpty colourEmpty
red :: Colour
red = Colour colourFull colourEmpty colourEmpty
green :: Colour
green = Colour colourEmpty colourFull colourEmpty
blue :: Colour
blue = Colour colourEmpty colourEmpty colourFull
yellow :: Colour
yellow = Colour colourFull colourFull colourEmpty
magenta :: Colour
magenta = Colour colourFull colourEmpty colourFull
cyan :: Colour
cyan = Colour colourEmpty colourFull colourFull
white :: Colour
white = Colour colourFull colourFull colourFull

setColour :: Colour -> IO ()
setColour (Colour r g b) = GL.color $
    GL.Color3 (rToGLfloat r) (rToGLfloat g) (rToGLfloat b)

setPoint :: Point -> IO ()
setPoint (Point x y) = GL.vertex $
    GL.Vertex3 (rToGLfloat x) (rToGLfloat y) (0::GL.GLfloat)

draw :: Shape a => Colour -> a -> IO ()
draw c s = do
    setColour c
    drawShape s

