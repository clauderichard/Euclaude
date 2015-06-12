module TriangleProperties.Relative
where

import qualified TriangleProperties.Abstract as Abstract

-- Here the triangle's sidelengths are known,
--   but its corners' positions are not.

--------------------------------------------------------------------------------

type R = Double

-- A point is defined by a triple of weights to use in
--   a weighted average of the triangle's corners.
data Point = Point (R,R,R)

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

getR :: (R,R,R) -> Abstract.R -> R
getR ls (Abstract.R f) = f ls

getPoint :: (R,R,R) -> Abstract.Point -> Point
getPoint (a,b,c) (Abstract.Point (Abstract.R f)) = Point (f(a,b,c), f(b,c,a), f(c,a,b))

getLine :: (R,R,R) -> Abstract.Line -> Line
getLine ls (Abstract.Line a b) = Line (getPoint ls a) (getPoint ls b)

getTriangle :: (R,R,R) -> Abstract.Triangle -> Triangle
getTriangle (a,b,c) (Abstract.Triangle (Abstract.R f) (Abstract.R g)) =
    Triangle (Point (f(a,b,c), g(a,b,c), g(a,c,b)))
             (Point (g(b,a,c), f(b,c,a), g(b,c,a)))
             (Point (g(c,a,b), g(c,b,a), f(c,b,a)))

getCircle :: (R,R,R) -> Abstract.Circle -> Circle
getCircle ls (Abstract.Circle c r) = Circle (getPoint ls c) (getR ls r)

getR3 :: (R,R,R) -> Abstract.R3 -> R3
getR3 (a,b,c) (Abstract.R3 (Abstract.R f)) = R3 (f (a,b,c)) (f (b,c,a)) (f (c,a,b))

getTriangle3 :: (R,R,R) -> Abstract.Triangle3 -> Triangle3
getTriangle3 ls (Abstract.Triangle3 t1 t2 t3) =
    Triangle3 (Triangle a1 b1 c1) (Triangle a2 b2 c2) (Triangle a3 b3 c3)
  where Triangle a1 a2 a3 = getTriangle ls t1
        Triangle b1 b2 b3 = getTriangle ls t2
        Triangle c1 c2 c3 = getTriangle ls t3

--------------------------------------------------------------------------------
