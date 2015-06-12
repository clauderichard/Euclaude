module TriangleProperties.Abstract
where

-- In here, everything is defined as being a function of a positionless triangle.
--   Takes one argument (a triple with 3 sidelengths of a triangle)
--   and returns something.

--------------------------------------------------------------------------------

-- R represents a function taking 3 sidelengths of a triangle,
--   in order, and returns a real number.
data R = R ((Double,Double,Double) -> Double)

instance Num R where
    fromInteger n = R $ \x -> fromInteger n
    (R f) + (R g) = R $ \x -> f x + g x
    (R f) - (R g) = R $ \x -> f x - g x
    (R f) * (R g) = R $ \x -> f x * g x
    abs (R f)     = R $ \x -> abs (f x)
    signum (R f)  = R $ \x -> signum (f x)
    
instance Fractional R where
    fromRational r = R $ \x -> fromRational r
    recip (R f)    = R $ \x -> recip (f x)
    (R f) / (R g)  = R $ \x -> f x / g x

instance Floating R where
    pi          = R $ \x -> pi
    exp (R f)   = R $ \x -> exp (f x)
    log (R f)   = R $ \x -> log (f x)
    sin (R f)   = R $ \x -> sin (f x)
    cos (R f)   = R $ \x -> cos (f x)
    asin (R f)  = R $ \x -> asin (f x)
    acos (R f)  = R $ \x -> acos (f x)
    atan (R f)  = R $ \x -> atan (f x)
    sinh (R f)  = R $ \x -> sinh (f x)
    cosh (R f)  = R $ \x -> cosh (f x)
    asinh (R f) = R $ \x -> asinh (f x)
    acosh (R f) = R $ \x -> acosh (f x)
    atanh (R f) = R $ \x -> atanh (f x)

-- Point is actually a triangle centre.
-- The only thing you really need is a function that takes the triangle's
--   sidelengths and gives the weight of corner A in a weighted average
--   of the triangle's corners.
-- The same function is used to figure out the weights of the other corners
--   by applying the function with the arguments reordered.
-- The function should be symmetric in the last 2 arguments.
data Point = Point R

-- A Line is defined by 2 endpoints. Simple as that.
data Line = Line Point Point

-- A Triangle is not composed of 3 centers,
--   but is rather defined with 2 functions taking the sidelengths
--   in order, and returning the weight of A, and the weight of B, respectively.
--   To get the weight of C, apply the second function
--   with the last 2 arguments reversed.
--   and these weights will generate the first corner of the new Triangle
--   by taking the weighted average of the triangle's corners.
-- To get the other corners, apply the functions with the arguments reordered.
data Triangle = Triangle R R

-- A circle is defined by its center and its radius.
data Circle = Circle Point R

-- Triangle3 represents 3 triangles, one for each rotation of the corners.
-- Careful! The 3 Triangle arguments are not the same triangles that you want!
-- Each of them has one corner in each of the triangles that you want.
data Triangle3 = Triangle3 Triangle Triangle Triangle

-- R3 represents 3 reals, one for each corner of the triangle.
-- You just need one function that returns the first real when applied to
--   the sidelengths in order, and then the other reals 
data R3 = R3 R

--------------------------------------------------------------------------------
