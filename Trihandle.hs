module Trihandle
where

import Shape

data Trihandle =
    Trihandle { trihandleTriangle :: Triangle
              , trihandleClicked :: Bool
              , trihandlePoint :: Point
              , trihandleGrabbed :: Int }

makeTrihandle :: Trihandle
makeTrihandle =
    Trihandle { trihandleTriangle = Triangle p1 p2 p3
              , trihandleClicked = False
              , trihandlePoint = Point 0.0 0.0
              , trihandleGrabbed = 0 }
  where p1 = Point (-0.5) (0.5)
        p2 = Point (0.5) (0.5)
        p3 = Point (0.0) (-0.5)

triangleMoveCorner :: Int -> Point -> Triangle -> Triangle
triangleMoveCorner 0 p t = t
triangleMoveCorner 1 p (Triangle a b c) = Triangle p b c
triangleMoveCorner 2 p (Triangle a b c) = Triangle a p c
triangleMoveCorner 3 p (Triangle a b c) = Triangle a b p

triangleClosest :: Point -> Triangle -> Int
triangleClosest p (Triangle a b c) = maxi3 (f p a) (f p b) (f p c)
  where f (Point x y) (Point w z) = sq (w-x) + sq (z-y)
        sq n = n*n
        maxi3 a b c
          | a>b = if b>c then 3 else 2
          | otherwise = if a>c then 3 else 1

trihandleMove :: Point -> Trihandle -> Trihandle
trihandleMove p th@Trihandle{ trihandleTriangle=t, trihandleGrabbed=g } =
    th { trihandlePoint = p, trihandleTriangle = triangleMoveCorner g p t }

trihandleDown :: Trihandle -> Trihandle
trihandleDown th@Trihandle{trihandlePoint=p, trihandleTriangle=t} =
    th { trihandleClicked = True, trihandleGrabbed = triangleClosest p t }

trihandleUp :: Trihandle -> Trihandle
trihandleUp th = th { trihandleClicked = False, trihandleGrabbed = 0 }

trihandleMoveDown :: Point -> Trihandle -> Trihandle
trihandleMoveDown p' th@Trihandle{trihandleTriangle=t} =
    th { trihandleTriangle = t' }
  where g' = triangleClosest p' t
        t' = triangleMoveCorner g' p' t

trihandleMoveUp :: Point -> Trihandle -> Trihandle
trihandleMoveUp p' th@Trihandle{trihandleTriangle=t} =
    th { trihandleTriangle = t' }
  where g' = triangleClosest p' t
        t' = triangleMoveCorner g' p' t
