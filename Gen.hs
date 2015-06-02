module Gen
where

import Shape



--------------------------------------------------------------------------------
-- Definition of generators (the data types are all wrappers of functions)
--------------------------------------------------------------------------------

-- These 2 types are just shorthand for
-- functions taking 3 and 6 real-valued arguments respectively,
-- and returning a real number.
type R3R = R -> R -> R -> R
type R6R = R -> R -> R -> R -> R -> R -> R

-- F3L f means f is applied to the lengths of the sides of the triangle.
-- F3A f means f is applied to the angles of the triangle.
-- F3LA f means f is applied to both of the above.
data F3 =
    F3L R3R    -- use lengths only
  | F3A R3R    -- use angles only
  | F3LA R6R   -- use angles and lengths

-- F2 is the same as F3 except that the function should be symmetric
-- in the last 2 arguments (a bit different for F2LA but whatever)
-- so that switching the last 2 arguments the function would return the same result.
data F2 =
    F2L R3R    -- use lengths only
  | F2A R3R    -- use angles only
  | F2LA R6R   -- use angles and lengths
  
-- Generator that can be used to generate a point out of a triangle
data GenPt = GenPt F2                  -- Triangle -> Point

-- diagonal first, rest second
-- Edit: what does this comment mean anyway?
-- Generator that can be used to generate 3 points out of a triangle
data GenPt3 = GenPt3 F2 F2             -- Triangle -> (Point,Point,Point)
-- Generator that can be used to generate a real number out of a triangle
data GenR = GenR F3                    -- Triangle -> R
-- Generator that can be used to generate 3 real numbers out of a triangle
data GenR3 = GenR3 F2                  -- Triangle -> (R,R,R)
-- Generator that can be used to generate a circle out of a triangle
data GenCirc = GenCirc GenPt GenR      -- Triangle -> Circle
-- Generator that can be used to generate 3 circles out of a triangle
data GenCirc3 = GenCirc3 GenPt3 GenR3  -- Triangle -> (Circle,Circle,Circle)

-- Take a F2 and evaluate the function on the triangle,
-- using the lengths, angles, or both, depending on the F2 choice.
f2Eval :: F2 -> Triangle -> (R,R,R)
f2Eval (F2L f) t = (f g h i, f h i g, f i g h)
  where (g,h,i) = triangleOppositeSideLengths t
f2Eval (F2A f) t = (f g h i, f h i g, f i g h)
  where (g,h,i) = triangleAngles t
f2Eval (F2LA f) t = (f a b c g h i, f b c a h i g, f c a b i g h)
  where (a,b,c) = triangleOppositeSideLengths t
        (g,h,i) = triangleAnglesFromLengths (a,b,c)
        
-- Same as f2Eval but for F3.
f3Eval :: F3 -> Triangle -> R
f3Eval (F3L f) t = f g h i
  where (g,h,i) = triangleOppositeSideLengths t
f3Eval (F3A f) t = f g h i
  where (g,h,i) = triangleAngles t
f3Eval (F3LA f) t = f a b c g h i
  where (a,b,c) = triangleOppositeSideLengths t
        (g,h,i) = triangleAnglesFromLengths (a,b,c)
--------------------------------------------------------------------------------
-- Definition of generators (functions)
--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- Some geometry functions
--------------------------------------------------------------------------------
triangleOppositeSideLengths :: Triangle -> (R,R,R)
triangleOppositeSideLengths (Triangle (Point a b) (Point c d) (Point e f)) =
    (sqrt (x1*x1 + y1*y1), sqrt (x2*x2 + y2*y2), sqrt (x3*x3 + y3*y3))
  where x1 = c-e
        y1 = d-f
        x2 = e-a
        y2 = f-b
        x3 = a-c
        y3 = b-d
triangleAnglesFromLengths :: (R,R,R) -> (R,R,R)
triangleAnglesFromLengths (a,b,c) = (f a b c, f b c a, f c a b)
  where f x y z = acos $ (y*y+z*z-x*x)/(2*y*z)
triangleAngles :: Triangle -> (R,R,R)
triangleAngles = triangleAnglesFromLengths . triangleOppositeSideLengths
pointsWeightedAverage :: [R] -> [Point] -> Point
pointsWeightedAverage rs ps = pointDivide (pointsSum rs ps) (sum rs)
  where pointDivide (Point a b) c = Point (a/c) (b/c)
        pointAdd (Point a b) (Point c d) = Point (a+c) (b+d)
        pointScale r (Point a b) = Point (r*a) (r*b)
        pointsSum rs ps = foldl pointAdd (Point 0 0) (zipWith pointScale rs ps)
triangleAreaFromLengths :: (R,R,R) -> R
triangleAreaFromLengths (a,b,c) = sqrt (s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c) / 2
--------------------------------------------------------------------------------
-- Some geometry functions
--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- Generate shapes using generators
-- If you have a generator, and an actual Triangle,
-- then you can use one of these functions to return the actual point or shape
-- that the generator is supposed to generate from the triangle.
--------------------------------------------------------------------------------
point :: GenPt -> Triangle -> Point
point (GenPt f) t@(Triangle a b c) = pointsWeightedAverage [x,y,z] [a, b, c]
  where (x,y,z) = f2Eval f t
  
triangle :: GenPt3 -> Triangle -> Triangle
triangle (GenPt3 f2 g2) t@(Triangle a b c) = Triangle x y z
  where (fa,fb,fc) = f2Eval f2 t
        (ga,gb,gc) = f2Eval g2 t
        x = pointsWeightedAverage [fa,gb,gc] [a,b,c]
        y = pointsWeightedAverage [ga,fb,gc] [a,b,c]
        z = pointsWeightedAverage [ga,gb,fc] [a,b,c]
        
points :: GenPt3 -> Triangle -> [Point]
points g t = [x,y,z]
  where Triangle x y z = triangle g t

-- Given 2 generators, one for the start point of a line,
-- and the other generator is for the end point of each line,
-- return 
lineSegment :: GenPt -> GenPt -> Triangle -> Line
lineSegment f g t = Line (point f t) (point g t)

lineSegments :: GenPt3 -> GenPt3 -> Triangle -> [Line]
lineSegments f g t = [Line x1 x2, Line y1 y2, Line z1 z2]
  where Triangle x1 y1 z1 = triangle f t
        Triangle x2 y2 z2 = triangle g t

rays :: GenPt -> GenPt3 -> Triangle -> [Line]
rays f g t = [Line p x, Line p y, Line p z]
  where p = point f t
        Triangle x y z = triangle g t

raysMidpoints :: GenPt -> GenPt3 -> Triangle -> [Point]
raysMidpoints f g t = [pointsWeightedAverage [1,1] [p,x],
                       pointsWeightedAverage [1,1] [p,y],
                       pointsWeightedAverage [1,1] [p,z]]
  where p = point f t
        Triangle x y z = triangle g t

inscribedTriangle :: GenPt -> Triangle -> Triangle
inscribedTriangle (GenPt f2) t@(Triangle a b c) = Triangle d e f
  where (g,h,i) = f2Eval f2 t
        d = pointsWeightedAverage [h,i] [b,c]
        e = pointsWeightedAverage [i,g] [c,a]
        f = pointsWeightedAverage [g,h] [a,b]
  
cevians :: GenPt -> Triangle -> [Line]
cevians g t@(Triangle a b c) = [Line a d, Line b e, Line c f]
  where Triangle d e f = inscribedTriangle g t

cevianIntersects :: GenPt -> Triangle -> [Point]
cevianIntersects g t = [x,y,z]
  where Triangle x y z = inscribedTriangle g t

extendedCevians :: GenPt3 -> Triangle -> [Line]
extendedCevians g t@(Triangle a b c) = [Line a d, Line b e, Line c f]
  where Triangle d e f = triangle g t

radius :: GenR -> Triangle -> R
radius (GenR f3) t = f3Eval f3 t

radii :: GenR3 -> Triangle -> [R]
radii (GenR3 f2) t = [x,y,z]
  where (x,y,z) = f2Eval f2 t

circle :: GenPt -> GenR -> Triangle -> Circle
circle gp gr t = Circle c r
  where c = point gp t
        r = radius gr t
        
circles :: GenPt3 -> GenR3 -> Triangle -> [Circle]
circles gp gr t = [Circle x r1, Circle y r2, Circle z r3]
  where [x,y,z] = points gp t
        [r1,r2,r3] = radii gr t
--------------------------------------------------------------------------------
-- generate shapes using generators
--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- How to make a generator
--------------------------------------------------------------------------------
genPtL :: R3R -> GenPt
genPtL = GenPt . F2L
genPtA :: R3R -> GenPt
genPtA = GenPt . F2A
genPtLA :: R6R -> GenPt
genPtLA = GenPt . F2LA

genPt3L :: R3R -> R3R -> GenPt3
genPt3L f g = GenPt3 (F2L f) (F2L g)
genPt3A :: R3R -> R3R -> GenPt3
genPt3A f g = GenPt3 (F2A f) (F2A g)
genPt3LA :: R6R -> R6R -> GenPt3
genPt3LA f g = GenPt3 (F2LA f) (F2LA g)

genRL :: R3R -> GenR
genRL = GenR . F3L
genRA :: R3R -> GenR
genRA = GenR . F3A
genRLA :: R6R -> GenR
genRLA = GenR . F3LA

genR3L :: R3R -> GenR3
genR3L = GenR3 . F2L
genR3A :: R3R -> GenR3
genR3A = GenR3 . F2A
genR3LA :: R6R -> GenR3
genR3LA = GenR3 . F2LA

gCevianIntersects :: GenPt -> GenPt3
gCevianIntersects (GenPt (F2L g)) = genPt3L (\a b c -> 0) g
gCevianIntersects (GenPt (F2A g)) = genPt3A (\a b c -> 0) g
gCevianIntersects (GenPt (F2LA g)) = genPt3LA (\a b c d e f -> 0) g

gId :: GenPt3
gId = genPt3L (\a b c -> 1) (\a b c -> 0)
--------------------------------------------------------------------------------
-- How to make a generator
--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------
gCentroid = genPtL (\a b c -> 1)
gIncenter = genPtL (\a b c -> a)
gInradius = genRL (\a b c -> 2 * triangleAreaFromLengths(a,b,c) / (a+b+c))
gOutcenters = genPt3L (\a b c -> -a) (\a b c -> a)
gOutradii = genR3L (\a b c -> 2 * triangleAreaFromLengths(a,b,c) / (b+c-a))
gCircumcenter = genPtA (\a b c -> sin(2*a))
gCircumradius = genRL (\a b c -> a*b*c / (4 * triangleAreaFromLengths(a,b,c)))
gOrthocenter = genPtA (\a b c -> tan a)
gNinePointCenter = genPtLA (\a b c d e f -> a*cos(e-f))
gNinePointRadius = genRL (\a b c -> a*b*c / (8 * triangleAreaFromLengths(a,b,c)))
gSymmedian = genPtL (\a b c -> a*a)
gGergonne = genPtL (\a b c -> 1/(b+c-a))
gNagel = genPtL (\a b c -> b+c-a)
gMittenpunkt = genPtL (\a b c -> a*(b+c-a))
gSpieker = genPtL (\a b c -> b+c)
gFeuerbach = genPtL (\a b c -> (b+c-a)*(b-c)*(b-c))
gNapoleonPoint1 = genPtLA (\a b c d e f -> a/sin(d+pi/6))
gExeter = genPtL (\a b c -> a^2 * (b^4 + c^4 - a^4))

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------