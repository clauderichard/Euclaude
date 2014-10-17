module Shape
where

type R = Double
data Point = Point R R
data Line = Line Point Point
data Triangle = Triangle Point Point Point
data Tetragon = Tetragon Point Point Point Point
data Pentagon = Pentagon Point Point Point Point Point
data Circle = Circle Point R


linePoints :: Line -> [Point]
linePoints (Line a b) = [a,b]

pointsToLine :: [Point] -> Line
pointsToLine [a,b] = Line a b
pointsToTriangle :: [Point] -> Triangle
pointsToTriangle [a,b,c] = Triangle a b c
pointsToTetragon :: [Point] -> Tetragon
pointsToTetragon [a,b,c,d] = Tetragon a b c d
pointsToPentagon [a,b,c,d,e] = Pentagon a b c d e

triangleOppositeLines :: Triangle -> [Line]
triangleOppositeLines (Triangle a b c) = [Line b c, Line c a, Line a b]


-- Polygons

class Polygon a where
  corners :: a -> [Point]
  fromCorners :: [Point] -> a
  
instance Polygon Triangle where
  corners (Triangle a b c) = [a,b,c]
  fromCorners (a:b:c:_) = Triangle a b c
instance Polygon Tetragon where
  corners (Tetragon a b c d) = [a,b,c,d]
  fromCorners (a:b:c:d:_) = Tetragon a b c d
instance Polygon Pentagon where
  corners (Pentagon a b c d e) = [a,b,c,d,e]
  fromCorners (a:b:c:d:e:_) = Pentagon a b c d e