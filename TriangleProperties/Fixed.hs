module TriangleProperties.Fixed
where

import qualified TriangleProperties.Abstract as Abstract
import qualified TriangleProperties.Relative as Relative

-- Here the triangle's corner positions are known.

--------------------------------------------------------------------------------

type R = Double

-- A point is defined by a triple of weights to use in
--   a weighted average of the triangle's corners.
data Point = Point R R

-- A line is defined by 2 endpoints.
data Line = Line Point Point

-- Here a Triangle is defined by 3 points. Simple.
-- But the 3 points are still defined only by weight-triples.
data Triangle = Triangle Point Point Point

-- A circle is defined by its centre and radius.
data Circle = Circle Point R

-- R3 is just 3 reals.
data R3 = R3 R R R

-- Might as well define Triangle3 by 3 triangles.
data Triangle3 = Triangle3 Triangle Triangle Triangle

--------------------------------------------------------------------------------

-- Various functions to go from Abstract to Relative
--   given the sidelengths of the triangle.

getR :: Triangle -> Relative.R -> R
getR t = id

getPoint :: Triangle -> Relative.Point -> Point
getPoint (Triangle x y z) (Relative.Point (g,h,i)) = pointWavg [g,h,i] [x,y,z]
  where pointWavg rs ps = pointDivide (pointsSum rs ps) (sum rs)
        pointDivide (Point a b) c = Point (a/c) (b/c)
        pointAdd (Point a b) (Point c d) = Point (a+c) (b+d)
        pointScale r (Point a b) = Point (r*a) (r*b)
        pointsSum rs ps = foldl pointAdd (Point 0 0) (zipWith pointScale rs ps)

getLine :: Triangle -> Relative.Line -> Line
getLine t (Relative.Line a b) = Line (getPoint t a) (getPoint t b)

getTriangle :: Triangle -> Relative.Triangle -> Triangle
getTriangle t (Relative.Triangle a b c) =
    Triangle (getPoint t a) (getPoint t b) (getPoint t c)

getCircle :: Triangle -> Relative.Circle -> Circle
getCircle t (Relative.Circle c r) = Circle (getPoint t c) (getR t r)

getR3 :: Triangle -> Relative.R3 -> R3
getR3 t (Relative.R3 a b c) = R3 a b c

getTriangle3 :: Triangle -> Relative.Triangle3 -> Triangle3
getTriangle3 t (Relative.Triangle3 t1 t2 t3) =
    Triangle3 (getTriangle t t1) (getTriangle t t2) (getTriangle t t3)

--------------------------------------------------------------------------------

