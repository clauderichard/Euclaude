--------------------------------------------------------------------------------
-- A model that helps with the interface between the mouse and a triangle
--------------------------------------------------------------------------------

module Trihandle
where

import Shape
import Geo

mouseDist2Restriction :: R
mouseDist2Restriction = 0.3

data Polyhandle = 
    Polyhandle { polyhandlePolygon :: [Point]
               , polyhandleGrabbed :: Int }
               
polyhandleTriangle :: Polyhandle -> Triangle
polyhandleTriangle (Polyhandle { polyhandlePolygon = (a:b:c:xs) }) =
    Triangle a b c
    
polyhandlePentagon :: Polyhandle -> Pentagon
polyhandlePentagon (Polyhandle { polyhandlePolygon = (a:b:c:d:e:xs) }) =
    Pentagon a b c d e

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

makePolyhandle5 :: Polyhandle
makePolyhandle5 =
    Polyhandle { polyhandlePolygon = [p1,p2,p3,p4,p5]
              , polyhandleGrabbed = 0 }
  where p1 = Point (-0.5) (-0.8)
        p2 = Point (0.0) (0.5)
        p3 = Point (0.5) (-0.8)
        p4 = Point (-0.8) (0.0)
        p5 = Point (0.8) (0.0)

triangleMoveCorner :: Int -> Point -> Triangle -> Triangle
triangleMoveCorner 0 p t = t
triangleMoveCorner 1 p (Triangle a b c) = Triangle p b c
triangleMoveCorner 2 p (Triangle a b c) = Triangle a p c
triangleMoveCorner 3 p (Triangle a b c) = Triangle a b p

polygonMoveCorner :: Int -> Point -> [Point] -> [Point]
polygonMoveCorner 0 p ls = ls
polygonMoveCorner i p ls = f (i-1) ls
  where f 0 (x:xs) = p : xs
        f i (x:xs) = x : f (i-1) xs
        f i [] = []

-- returns closest point as long as its distance squared to mouse is less than 1st arg.
triangleClosestRestricted :: R -> Point -> Triangle -> Int
triangleClosestRestricted maxDist p t = if satis then i else 0
  where (i,cl) = triangleClosest2 p t
        satis = cl <= maxDist*maxDist

-- returns closest point as long as its distance squared to mouse is less than 1st arg.
polygonClosestRestricted :: R -> Point -> [Point] -> Int
polygonClosestRestricted maxDist p ps = if satis then i else 0
  where (i,cl) = polygonClosest2 p ps
        satis = cl <= maxDist

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

-- returns (index,dist) (distance not squared!)
polygonClosest2 :: Point -> [Point] -> (Int,R)
polygonClosest2 p ps = f $ map (pointDistance p) ps
  where f (l:ls) = g 0 l 1 ls
        g curmi curm curi [] = (curmi+1,curm)
        g curmi curm curi (x:xs)
          | curm > x  = g curi x (curi+1) xs
          | otherwise = g curmi curm (curi+1) xs

trihandleMove :: Point -> Trihandle -> Trihandle
trihandleMove p' th@Trihandle{trihandleTriangle=t,trihandleGrabbed=g} =
    th { trihandleTriangle = triangleMoveCorner g p' t
       , trihandleGrabbed = g }

polyhandleMove :: Point ->Polyhandle -> Polyhandle
polyhandleMove p' ph@Polyhandle{polyhandlePolygon=t,polyhandleGrabbed=g} =
    ph { polyhandlePolygon = polygonMoveCorner g p' t
       , polyhandleGrabbed = g }

trihandleDown :: Point -> Trihandle -> Trihandle
trihandleDown p' th@Trihandle{trihandleGrabbed=g, trihandleTriangle=t} =
    th { trihandleTriangle = triangleMoveCorner g' p' t
       , trihandleGrabbed = g' }
  where g' = triangleClosestRestricted mouseDist2Restriction p' t

polyhandleDown :: Point -> Polyhandle -> Polyhandle
polyhandleDown p' th@Polyhandle{polyhandleGrabbed=g, polyhandlePolygon=t} =
    th { polyhandlePolygon = polygonMoveCorner g' p' t
       , polyhandleGrabbed = g' }
  where g' = polygonClosestRestricted mouseDist2Restriction p' t

trihandleUp :: Point -> Trihandle -> Trihandle
trihandleUp p' th@Trihandle{trihandleTriangle=t,trihandleGrabbed=g} =
    th { trihandleTriangle = t
       , trihandleGrabbed = 0 }

polyhandleUp :: Point -> Polyhandle -> Polyhandle
polyhandleUp p' th@Polyhandle{polyhandlePolygon=t,polyhandleGrabbed=g} =
    th { polyhandlePolygon = t
       , polyhandleGrabbed = 0 }
