--------------------------------------------------------------------------------
-- Contains functions to generate properties of geometric shapes
-- (especially properties of a triangle)
--------------------------------------------------------------------------------

module Geo
where

import Shape


data IsocelesTriangleType =
    IsocelesEquilateralTriangleType
  | IsocelesRightTriangleType

pointsWeightedAverage :: [R] -> [Point] -> Point
pointsWeightedAverage rs ps = pointDivide (pointsSum rs ps) (sum rs)
  where pointDivide (Point a b) c = Point (a/c) (b/c)
        pointAdd (Point a b) (Point c d) = Point (a+c) (b+d)
        pointScale r (Point a b) = Point (r*a) (r*b)
        pointsSum rs ps = foldl pointAdd (Point 0 0) (zipWith pointScale rs ps)

pointsAverage :: [Point] -> Point
pointsAverage = pointsWeightedAverage $ repeat 1

pointDistance :: Point -> Point -> R
pointDistance (Point a b) (Point c d) = sqrt (x*x + y*y)
  where x = c - a
        y = d - b
          
lineLength :: Line -> R
lineLength (Line a b) = pointDistance a b

-- pointRotate angle about point
pointRotate :: R -> Point -> Point -> Point
pointRotate angle (Point a b) (Point p q) = Point x y
  where c = cos angle
        s = sin angle
        pa = p - a
        qb = q - b
        x = a + c*pa - s*qb
        y = b + s*pa + c*qb
        
linesCrossProduct :: Line -> Line -> R
linesCrossProduct (Line (Point a b) (Point c d)) (Line (Point e f) (Point g h))
  = (c-a)*(h-f) - (b-d)*(g-e)

linesIntersection :: Line -> Line -> Point
linesIntersection l1@(Line a b) l2@(Line c d)
  = let t = linesCrossProduct (Line c d) (Line a c)
        u = linesCrossProduct (Line c d) (Line c b)
    in pointsWeightedAverage [t,u] [b,a]

--linesExtendToIntersection :: Line -> Line -> (Line,Line)
--linesExtendToIntersection
        
--isClockwise :: Triangle -> Bool
--isClockwise (Triangle a b c) = 
        
triangleOppositeSideLengths :: Triangle -> [R]
triangleOppositeSideLengths = map lineLength . triangleOppositeLines

-- match the respective corners of 2 triangles, gives 3 lines
lineupTriangles :: Triangle -> Triangle -> [Line]
lineupTriangles (Triangle a b c) (Triangle d e f) = [Line a d, Line b e, Line c f]
          
type CevianGenerator = R -> R -> R -> R
type CevianExtender = R -> R -> R -> R
type Claudian = (CevianExtender,CevianGenerator)

claudianPoint :: Claudian -> Triangle -> Point
claudianPoint cl t@(Triangle a b c) = pointsWeightedAverage weights [a,b,c]
  where claudianWeights :: Claudian -> [R] -> [R]
        claudianWeights (ce,cg) [g,h,i] = [ce g h i, cg h i g, cg i g h]
        weights = claudianWeights cl $ triangleOppositeSideLengths t
claudianTriangle :: Claudian -> Triangle -> Triangle
claudianTriangle (ce,cg) t@(Triangle a b c) = Triangle d e f
  where [g,h,i] = triangleOppositeSideLengths t
        fa = ce g h i
        fb = ce h i g
        fc = ce i g h
        ga = cg g h i
        gb = cg h i g
        gc = cg i g h
        d = pointsWeightedAverage [fa,gb,gc] [a,b,c]
        e = pointsWeightedAverage [ga,fb,gc] [a,b,c]
        f = pointsWeightedAverage [ga,gb,fc] [a,b,c]
claudianLines :: Claudian -> Triangle -> [Line]
claudianLines cl t = lineupTriangles t $ claudianTriangle cl t
claudianConcurrency :: Claudian -> Triangle -> Point
claudianConcurrency (ce,cg) t@(Triangle a b c) =
    pointsWeightedAverage [cg g h i, cg h i g, cg i g h] [a,b,c]
  where [g,h,i] = triangleOppositeSideLengths t

ceCevian :: CevianExtender
ceCevian _ _ _ = 0
cevianPoint :: CevianGenerator -> Triangle -> Point
cevianPoint cg = claudianPoint (ceCevian,cg)
cevianTriangle :: CevianGenerator -> Triangle -> Triangle
cevianTriangle cg = claudianTriangle (ceCevian,cg)
cevianConcurrency :: CevianGenerator -> Triangle -> Point
cevianConcurrency cg = claudianConcurrency (ceCevian,cg)
cevianLines :: CevianGenerator -> Triangle -> [Line]
cevianLines cg = claudianLines (ceCevian,cg)
        
cgMedian :: CevianGenerator
cgMedian _ _ _ = 1
triangleMedians :: Triangle -> [Line]
triangleMedians = cevianLines cgMedian
triangleCentroid :: Triangle -> Point
triangleCentroid = cevianConcurrency cgMedian

-- Escribed circle centers: (-a,b,c), (a,-b,c), (a,b,-c)
clOutcenters :: Claudian
clOutcenters = (\a b c -> -a, \a b c -> a)
triangleOutcenters :: Triangle -> [Point]
triangleOutcenters = corners . claudianTriangle clOutcenters

-- Nagel point
cgNagel :: CevianGenerator
cgNagel a b c = b + c - a
triangleNagelPoint :: Triangle -> Point
triangleNagelPoint = cevianConcurrency cgNagel

cgIncenter :: CevianGenerator
cgIncenter a _ _ = a
triangleAngleBisectors :: Triangle -> [Line]
triangleAngleBisectors = cevianLines cgIncenter
triangleIncenter :: Triangle -> Point
triangleIncenter = cevianConcurrency cgIncenter

cgCircumcenter :: CevianGenerator
cgCircumcenter a b c = b*b + c*c - a*a
triangleCircumcenter :: Triangle -> Point
triangleCircumcenter = cevianConcurrency cgCircumcenter

cgOrthocenter :: CevianGenerator
cgOrthocenter a b c = 1 / (b*b + c*c - a*a)
triangleHeights :: Triangle -> [Line]
triangleHeights = cevianLines cgOrthocenter
triangleOrthocenter :: Triangle -> Point
triangleOrthocenter = cevianConcurrency cgOrthocenter

cgSymmedian :: CevianGenerator
cgSymmedian a _ _ = a*a
triangleSymmedians :: Triangle -> [Line]
triangleSymmedians = cevianLines cgSymmedian
triangleSymmedianPoint :: Triangle -> Point
triangleSymmedianPoint = cevianConcurrency cgSymmedian
        
completeIsocelesTriangle :: R -> Point -> Point -> Point
completeIsocelesTriangle heightOverHalfBase (Point a b) (Point p q) = Point x y
  where x = (p+a)/2 - heightOverHalfBase*(q-b)/2
        y = (q+b)/2 + heightOverHalfBase*(p-a)/2
completeEquilateralTriangle :: Point -> Point -> Point
completeEquilateralTriangle = completeIsocelesTriangle $ sqrt 3
completeRightTriangle :: Point -> Point -> Point
completeRightTriangle = completeIsocelesTriangle 1
--triangleSideEquilateralTriangleCorners :: Bool -> Triangle -> [Point]
--triangleSideEquilateralTriangleCorners outer t@(Triangle a b c) = 
--    if normal
--    then [ completeEquilateralTriangle b c
--         , completeEquilateralTriangle c a
--         , completeEquilateralTriangle a b]
--    else [ completeEquilateralTriangle b c
--         , completeEquilateralTriangle c a
--         , completeEquilateralTriangle a b]
--  where normal = isClockwise t && outer
--        func = if normal 
--               then completeEquilateralTriangle 
--               else flip completeEquilateralTriangle
--        ta = func b c
--        tb = func c a
--        tc = func a b
