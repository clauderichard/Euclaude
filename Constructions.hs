--------------------------------------------------------------------------------
-- Contains functions that take in some geometric shapes,
-- and output other geometric shapes.
-- (they "construct" things, in Euclidean geometry terms)
--------------------------------------------------------------------------------

module Constructions
where

import Shape
import Centres

--------------------------------------------------------------------------------

linesIntersect :: Line -> Line -> Point
linesIntersect (Line (Point x1 y1) (Point x2 y2)) (Line (Point x3 y3) (Point x4 y4)) =
    Point x y
  where a = y1-y2
        b = x2-x1
        c = y3-y4
        d = x4-x3
        det = a*d - b*c
        v1 = y1*x2-x1*y2
        v2 = y3*x4-x3*y4
        x = (d*v1-b*v2)/det
        y = (a*v2-c*v1)/det

ceviansThrough :: Triangle -> Point -> [Line]
ceviansThrough (Triangle pa pb pc) x = [Line pa ia, Line pb ib, Line pc ic]
  where ia = linesIntersect (Line pa x) (Line pb pc)
        ib = linesIntersect (Line pb x) (Line pc pa)
        ic = linesIntersect (Line pc x) (Line pa pb)
--------------------------------------------------------------------------------
