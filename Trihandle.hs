--------------------------------------------------------------------------------
-- A model that helps with the interface between the mouse and a triangle
--------------------------------------------------------------------------------

module Trihandle
where

import Shape

mouseDist2Restriction :: R
mouseDist2Restriction = 0.3

data Trihandle =
    Trihandle { trihandleTriangle :: Triangle
              , trihandleGrabbed :: Int }

makeTrihandle :: Trihandle
makeTrihandle =
    Trihandle { trihandleTriangle = Triangle p1 p2 p3
              , trihandleGrabbed = 0 }
  where p1 = Point (-0.5) (0.5)
        p2 = Point (0.5) (0.5)
        p3 = Point (0.0) (-0.5)

triangleMoveCorner :: Int -> Point -> Triangle -> Triangle
triangleMoveCorner 0 p t = t
triangleMoveCorner 1 p (Triangle a b c) = Triangle p b c
triangleMoveCorner 2 p (Triangle a b c) = Triangle a p c
triangleMoveCorner 3 p (Triangle a b c) = Triangle a b p

-- returns closest point as long as its distance squared to mouse is less than 1st arg.
triangleClosestRestricted :: R -> Point -> Triangle -> Int
triangleClosestRestricted maxDist p t = if satis then i else 0
  where (i,cl) = triangleClosest2 p t
        satis = cl <= maxDist*maxDist

-- returns index of triangle corner that is closest to mouse.
triangleClosest :: Point -> Triangle -> Int
triangleClosest p (Triangle a b c) = maxi3 (f p a) (f p b) (f p c)
  where f (Point x y) (Point w z) = sq (w-x) + sq (z-y)
        sq n = n*n
        maxi3 a b c
          | a>b = if b>c then 3 else 2
          | otherwise = if a>c then 3 else 1

-- returns (index,dist2) similar to above
triangleClosest2 :: Point -> Triangle -> (Int,R)
triangleClosest2 p (Triangle a b c) = min3 (f p a) (f p b) (f p c)
  where f (Point x y) (Point w z) = sq (w-x) + sq (z-y)
        sq n = n*n
        min3 a b c
          | a>b = if b>c then (3,c) else (2,b)
          | otherwise = if a>c then (3,c) else (1,a)

trihandleMove :: Point -> Trihandle -> Trihandle
trihandleMove p' th@Trihandle{trihandleTriangle=t,trihandleGrabbed=g} =
    th { trihandleTriangle = triangleMoveCorner g p' t
       , trihandleGrabbed = g }

trihandleDown :: Point -> Trihandle -> Trihandle
trihandleDown p' th@Trihandle{trihandleGrabbed=g, trihandleTriangle=t} =
    th { trihandleTriangle = triangleMoveCorner g' p' t
       , trihandleGrabbed = g' }
  where g' = triangleClosestRestricted mouseDist2Restriction p' t

trihandleUp :: Point -> Trihandle -> Trihandle
trihandleUp p' th@Trihandle{trihandleTriangle=t,trihandleGrabbed=g} =
    th { trihandleTriangle = t
       , trihandleGrabbed = 0 }
