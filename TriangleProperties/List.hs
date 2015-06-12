module TriangleProperties.List
where

import TriangleProperties.Abstract

-- List of all the cool triangle properties.

--------------------------------------------------------------------------------

-- Side lengths
a = R $ \(x,y,z) -> x
b = R $ \(x,y,z) -> y
c = R $ \(x,y,z) -> z

-- Corner angles
a' = acos $ (b*b+c*c-a*a)/(2*b*c)
b' = acos $ (c*c+a*a-b*b)/(2*c*a)
c' = acos $ (a*a+b*b-c*c)/(2*a*b)

area = sqrt $ s*(s-a)*(s-b)*(s-c)
  where s = (a+b+c)/2

-- Centres
centroid        = Point $ 1
incentre        = Point $ a
orthocentre     = Point $ tan a'
circumcentre    = Point $ sin (2 * a')
symmedianPoint  = Point $ a*a
ninePointCenter = Point $ a * cos(b'-c')
gergonnePoint   = Point $ 1/(b+c-a)
nagelPoint      = Point $ b+c-a
mittenpunkt     = Point $ a * (b+c-a)
spiekerPoint    = Point $ b + c
napoleonPoint1  = Point $ a / sin(a' + pi/6)
feuerbachPoint  = Point $ (b+c-a)*(b-c)*(b-c)
exeterPoint     = Point $ a^2 * (b^4 + c^4 - a^4)

-- Lengths
inradius        = 2 * area / (a+b+c)
circumradius    = a*b*c / (4 * area)
ninePointRadius = circumradius / 2

-- Circles
incircle     = Circle incentre     inradius
circumcircle = Circle circumcentre circumradius

outcenters = Triangle (-a) a
outradii = R3 (2 * area / (b+c-a))

--------------------------------------------------------------------------------

-- Generating lines and whatnot based on other properties!

--ceviansThrough :: Point -> Line3

--------------------------------------------------------------------------------
