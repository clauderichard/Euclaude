module Claudian
where

import Shape

--------------------------------------------------------------------------------

-- Shorthands for functions taking 3 and 6 real arguments respectively,
--   and returning a real.
type R3R = R -> R -> R -> R
type R6R = R -> R -> R -> R -> R -> R -> R

-- Given a R3R and a Triangle, evaluate the function with the side-lengths
--   of the Triangle.
r3rEvalTriangleL :: R3R -> Triangle -> R
r3rEvalTriangleL f t = f g h i
  where (g,h,i) = triangleLengths t

-- Given a R3R and a Triangle, evaluate the function with the angles
--   of the Triangle.
r3rEvalTriangleA :: R3R -> Triangle -> R
r3rEvalTriangleA f t = f g h i
  where (g,h,i) = triangleAngles t

-- Given a R6R and a Triangle, evaluate the function with the side-lengths and angles
--   of the Triangle.
r6rEvalTriangleLA :: R6R -> Triangle -> R
r6rEvalTriangleLA f t = f a b c g h i
  where (a,b,c,g,h,i) = triangleLengthsAndAngles t

triangleLengths :: Triangle -> (R,R,R)
triangleLengths (Triangle (Point a b) (Point c d) (Point e f)) =
    (sqrt (x1*x1 + y1*y1), sqrt (x2*x2 + y2*y2), sqrt (x3*x3 + y3*y3))
  where x1 = c-e
        y1 = d-f
        x2 = e-a
        y2 = f-b
        x3 = a-c
        y3 = b-d
triangleAngles :: Triangle -> (R,R,R)
triangleAngles t = (f a b c, f b c a, f c a b)
  where (a,b,c) = triangleLengths t
        f x y z = acos $ (y*y+z*z-x*x)/(2*y*z)
triangleLengthsAndAngles :: Triangle -> (R,R,R,R,R,R)
triangleLengthsAndAngles t =
    ( a, b, c, f a b c, f b c a, f c a b )
  where (a,b,c) = triangleLengths t
        f x y z = acos $ (y*y+z*z-x*x)/(2*y*z)
pointsWeightedAverage :: [R] -> [Point] -> Point
pointsWeightedAverage rs ps = pointDivide (pointsSum rs ps) (sum rs)
  where pointDivide (Point a b) c = Point (a/c) (b/c)
        pointAdd (Point a b) (Point c d) = Point (a+c) (b+d)
        pointScale r (Point a b) = Point (r*a) (r*b)
        pointsSum rs ps = foldl pointAdd (Point 0 0) (zipWith pointScale rs ps)
--------------------------------------------------------------------------------

-- For the L and A versions of CenterClaudian,
--   the function passed must be symmetric in its last 2 arguments.
--   Otherwise what is generated by this function may not even converge to one center.
-- L: Apply the function to lengths (a b c) in that order
--   to get the weight of point A.
-- A: Apply the function to angles (A B C) in that order
--   to get the weight of point A.
-- LA: Apply the function to lengths then angles (a b c A B C) in that order
--   to get the weight of point A.
      
data CenterClaudian =
    CenterClaudianL R3R
  | CenterClaudianA R3R
  | CenterClaudianLA R6R

applyCenterClaudian :: CenterClaudian -> Triangle -> Point
applyCenterClaudian (CenterClaudianL f) t@(Triangle a b c) =
    pointsWeightedAverage [f g h i, f h i g, f i g h] [a,b,c]
  where (g,h,i) = triangleLengths t

--------------------------------------------------------------------------------

-- Some examples. TODO: move this to another file full of triangle properties.

fCentroidL :: R3R
fCentroidL a b c = 1
centroidClaudian :: CenterClaudian
centroidClaudian = CenterClaudianL fCentroidL
centroid :: Triangle -> Point
centroid = applyCenterClaudian centroidClaudian

--------------------------------------------------------------------------------