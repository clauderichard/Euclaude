module Shape
where

-- R = real number
type R = Double

-- Bunch of shape data-types
data Point = Point R R
data Line = Line Point Point
data Triangle = Triangle Point Point Point
data Tetragon = Tetragon Point Point Point Point
data Pentagon = Pentagon Point Point Point Point Point
data Circle = Circle Point R

-- get the endpoints of a line
linePoints :: Line -> [Point]
linePoints (Line a b) = [a,b]

-- This is really used for vectors (difference between two points) but I will use Point type for vectors.
pointCrossProduct :: Point -> Point -> R
pointCrossProduct (Point a b) (Point c d) = a*d - b*c
        
-- Whether 3 points go counterclockwise or not
-- (used for constructing the outer Napoleon triangle for example)
isCounterClockwise :: Point -> Point -> Point -> Bool
isCounterClockwise a b c = pointCrossProduct (subp b a) (subp c a) > 0
  where subp (Point e f) (Point g h) = Point (e-g) (f-h)
        

-- from a list of points, get a shape.
-- The list has to have exactly the right number of entries.
pointsToLine :: [Point] -> Line
pointsToLine [a,b] = Line a b
pointsToTriangle :: [Point] -> Triangle
pointsToTriangle [a,b,c] = Triangle a b c
pointsToTetragon :: [Point] -> Tetragon
pointsToTetragon [a,b,c,d] = Tetragon a b c d
pointsToPentagon [a,b,c,d,e] = Pentagon a b c d e

-- From a triangle, get a list of its 3 sides as lines,
-- where if you match them to the corners in order,
-- each line in return value is geometrically opposite to the corresponding corner.
triangleOppositeLines :: Triangle -> [Line]
triangleOppositeLines (Triangle a b c) = [Line b c, Line c a, Line a b]


-- Polygons

-- class Polygon x allows you to go to and from a list of points and the data type x.
class Polygon a where
  corners :: a -> [Point]
  fromCorners :: [Point] -> a
  
-- instances of the Polygon class
instance Polygon Triangle where
  corners (Triangle a b c) = [a,b,c]
  fromCorners (a:b:c:_) = Triangle a b c
instance Polygon Tetragon where
  corners (Tetragon a b c d) = [a,b,c,d]
  fromCorners (a:b:c:d:_) = Tetragon a b c d
instance Polygon Pentagon where
  corners (Pentagon a b c d e) = [a,b,c,d,e]
  fromCorners (a:b:c:d:e:_) = Pentagon a b c d e