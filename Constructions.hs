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
        
cevianIntersects :: Triangle -> Point -> [Point]
cevianIntersects t x = map e $ ceviansThrough t x
  where e (Line a b) = b
        
rays :: Point -> [Point] -> [Line]
rays x ps = map (\p -> Line x p) ps

midpoint :: Line -> Point
midpoint (Line (Point a b) (Point c d)) = Point ((a+c)/2) ((b+d)/2)

raysMidpoints :: Point -> [Point] -> [Point]
raysMidpoints c ps = map midpoint $ rays c ps
        
--------------------------------------------------------------------------------

-- Given two points a and b, return c such that (a,b,c) are 3 corners of an equilateral triangle going counterclockwise.
completeEquilateralTriangle :: Point -> Point -> Point
completeEquilateralTriangle (Point a b) (Point c d) = Point e f
  where x = (c-a)/2 - (d-b)*(sqrt 3)/2
        y = (c-a)*(sqrt 3)/2 + (d-b)/2
        e = a + x
        f = b + y
        
-- Bool argument is true for outer, false for inner.
sideEquilateralTriangleCorners :: Bool -> Triangle -> [Point]
sideEquilateralTriangleCorners isouter (Triangle a b c)
  | isCounterClockwise a b c == isouter = [completeEquilateralTriangle c b, completeEquilateralTriangle a c, completeEquilateralTriangle b a]
  | otherwise                           = [completeEquilateralTriangle b c, completeEquilateralTriangle c a, completeEquilateralTriangle a b]
        
sideEquilateralTriangles :: Bool -> Triangle -> [Triangle]
sideEquilateralTriangles isouter t@(Triangle a b c) = [Triangle b c x, Triangle c a y, Triangle a b z]
  where [x,y,z] = sideEquilateralTriangleCorners isouter t

napoleonTriangle :: Bool -> Triangle -> Triangle
napoleonTriangle isouter t = pointsToTriangle $ map centroid $ sideEquilateralTriangles isouter t
  
outerEquilateralTriangleCorners :: Triangle -> [Point]
outerEquilateralTriangleCorners (Triangle a b c)
  | isCounterClockwise a b c = [completeEquilateralTriangle c b, completeEquilateralTriangle a c, completeEquilateralTriangle b a]
  | otherwise                = [completeEquilateralTriangle b c, completeEquilateralTriangle c a, completeEquilateralTriangle a b]

outerEquilateralTriangles :: Triangle -> [Triangle]
outerEquilateralTriangles t@(Triangle a b c) = [Triangle b c x, Triangle c a y, Triangle a b z]
  where [x,y,z] = outerEquilateralTriangleCorners t

outerNapoleonTriangle :: Triangle -> Triangle
outerNapoleonTriangle t = pointsToTriangle $ map centroid $ outerEquilateralTriangles t
  
--------------------------------------------------------------------------------
