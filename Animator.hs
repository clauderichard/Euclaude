module Animator
where

import Shape

data TriangleAnimator = TriangleAnimator1 R
  | TriangleAnimator2 R (R,R)

makeTriangleAnimator :: TriangleAnimator
makeTriangleAnimator = TriangleAnimator2 0.0 (0.0,0.0)
--makeTriangleAnimator = TriangleAnimator1 0.0

moveTriangleAnimator :: (R,R) -> TriangleAnimator -> TriangleAnimator
moveTriangleAnimator (x,y) (TriangleAnimator2 t (a,b)) = TriangleAnimator2 t (x,y)

stepTriangleAnimator :: R -> TriangleAnimator -> TriangleAnimator
stepTriangleAnimator t (TriangleAnimator1 x) = TriangleAnimator1 (x+t/3.0)
stepTriangleAnimator t (TriangleAnimator2 x (a,b)) = TriangleAnimator2 (x+t/3.0) (a,b)

animatedTriangle :: TriangleAnimator -> Triangle
animatedTriangle (TriangleAnimator1 x) = Triangle a b c
  where ta = x
        tb = x + pi*2.0/3
        tc = x - pi*2.0/3
        a = Point (sin $ 2*ta) (cos ta)
        b = Point (sin $ 3*tb) (cos tb)
        c = Point (sin tc) (cos tc)
animatedTriangle (TriangleAnimator2 x (p,q)) = Triangle a b c
  where ta = x
        tb = x + pi*2.0/3
        tc = x - pi*2.0/3
        r = 0.7
        a = Point (p + r*sin (2*ta)) (q + r*cos ta)
        b = Point (p + r*sin (3*tb)) (q + r*cos tb)
        c = Point (p + r*sin (1*tc)) (q + r*cos tc)