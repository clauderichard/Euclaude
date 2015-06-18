module Centres
( centroid
, incentre
, orthocentre
, circumcentre
, ninepointcentre
, symmedianpoint
, gergonnepoint
, nagelpoint
, mittenpunkt
, spiekerpoint
, feuerbachpoint
, napoleonpoint1
, exeterpoint
) where

import qualified Shape as S

--------------------------------------------------------------------------------

centroid        = centre $ 1
incentre        = centre $ a
orthocentre     = centre $ tan a'
circumcentre    = centre $ sin $ 2*a'
ninepointcentre = centre $ a * cos (b'-c')
symmedianpoint  = centre $ a*a
gergonnepoint   = centre $ recip $ b+c-a
nagelpoint      = centre $ b+c-a
mittenpunkt     = centre $ a*(b+c-a)
spiekerpoint    = centre $ b+c
feuerbachpoint  = centre $ (b+c-a)*(b-c)*(b-c)
napoleonpoint1  = centre $ a / sin (a' + pi/6)
exeterpoint     = centre $ a^2 * (b^4 + c^4 - a^4)

circumradius    = radius $ a*b*c / (4 * area)
inradius        = radius $ 2 * area / (a+b+c)
ninepointradius = radius $ a*b*c / (8 * area)

outcentres = centres3 (-a) a

outradii = radii3 $ 2 * area / (b+c-a)

circumcircle    = circle circumcentre    circumradius
incircle        = circle incentre        inradius
ninepointcircle = circle ninepointcentre ninepointradius

outcircles = circles3 outcentres outradii

--------------------------------------------------------------------------------

-- Represents a function from the triangle sidelengths to another real number.
data R x = R (x -> x -> x -> x)

instance (Num x) => Num (R x) where
    fromInteger i = R $ \x y z -> fromInteger i
    (R f) + (R g) = R $ \x y z -> f x y z + g x y z
    (R f) - (R g) = R $ \x y z -> f x y z - g x y z
    (R f) * (R g) = R $ \x y z -> f x y z * g x y z
    abs (R f)     = R $ \x y z -> abs (f x y z)
    signum (R f)  = R $ \x y z -> signum (f x y z)

instance (Fractional x) => Fractional (R x) where
    fromRational r = R $ \x y z -> fromRational r
    (R f) / (R g)  = R $ \x y z -> f x y z / g x y z

instance (Floating x) => Floating (R x) where
    pi          = R $ \x y z -> pi
    exp   (R f) = R $ \x y z -> exp (f x y z)
    log   (R f) = R $ \x y z -> log (f x y z)
    sin   (R f) = R $ \x y z -> sin (f x y z)
    cos   (R f) = R $ \x y z -> cos (f x y z)
    asin  (R f) = R $ \x y z -> asin (f x y z)
    acos  (R f) = R $ \x y z -> acos (f x y z)
    atan  (R f) = R $ \x y z -> atan (f x y z)
    sinh  (R f) = R $ \x y z -> sinh (f x y z)
    cosh  (R f) = R $ \x y z -> cosh (f x y z)
    asinh (R f) = R $ \x y z -> asinh (f x y z)
    acosh (R f) = R $ \x y z -> acosh (f x y z)
    atanh (R f) = R $ \x y z -> atanh (f x y z)

--------------------------------------------------------------------------------

a :: (Floating x) => R x
a = R $ \x y z -> x
b :: (Floating x) => R x
b = R $ \x y z -> y
c :: (Floating x) => R x
c = R $ \x y z -> z

a' :: (Floating x) => R x
a' = acos $ (b*b+c*c-a*a)/(2*b*c)
b' :: (Floating x) => R x
b' = acos $ (c*c+a*a-b*b)/(2*c*a)
c' :: (Floating x) => R x
c' = acos $ (a*a+b*b-c*c)/(2*a*b)

area :: (Floating x) => R x
area = sqrt $ s*(s-a)*(s-b)*(s-c)
  where s = (a+b+c)/2

--------------------------------------------------------------------------------

pwavg :: [S.R] -> [S.Point] -> S.Point
pwavg rs ps = pdiv (psum ps) (sum rs)
  where psum = foldl padd porigin
        pdiv (S.Point a b) d = S.Point (a/d) (b/d)
        padd (S.Point a b) (S.Point c d) = S.Point (a+c) (b+d)
        porigin = S.Point 0 0
        
pdist :: S.Point -> S.Point -> S.R
pdist (S.Point a b) (S.Point c d) = sqrt $ (a-c)*(a-c) + (b-d)*(b-d)

centre :: R S.R -> S.Triangle -> S.Point
centre (R f) (S.Triangle a b c) = pwavg [f la lb lc, f lb lc la, f lc la lb] [a,b,c]
  where (la,lb,lc) = (pdist b c, pdist c a, pdist a b)

radius :: R S.R -> S.Triangle -> S.R
radius (R f) (S.Triangle a b c) = f la lb lc
  where (la,lb,lc) = (pdist b c, pdist c a, pdist a b)

centres3 :: R S.R -> R S.R -> S.Triangle -> [S.Point]
centres3 (R fa) (R fbc) (S.Triangle a b c) =
    [ pwavg [fa la lb lc, fbc lb lc la, fbc lc la lb] [a,b,c]
    , pwavg [fa lb lc la, fbc lc la lb, fbc la lb lc] [b,c,a]
    , pwavg [fa lc la lb, fbc la lb lc, fbc lb lc la] [c,a,b] ]
  where (la,lb,lc) = (pdist b c, pdist c a, pdist a b)

radii3 :: R S.R -> S.Triangle -> [S.R]
radii3 (R f) (S.Triangle a b c) = [f la lb lc, f lb lc la, f lc la lb]
  where (la,lb,lc) = (pdist b c, pdist c a, pdist a b)

circle :: (S.Triangle -> S.Point) -> (S.Triangle -> S.R) -> S.Triangle -> S.Circle
circle c r t = S.Circle (c t) (r t)

circles3 :: (S.Triangle -> [S.Point]) -> (S.Triangle -> [S.R]) -> S.Triangle -> [S.Circle]
circles3 cs rs t = [S.Circle c1 r1, S.Circle c2 r2, S.Circle c3 r3]
  where [c1,c2,c3] = cs t
        [r1,r2,r3] = rs t

--------------------------------------------------------------------------------
