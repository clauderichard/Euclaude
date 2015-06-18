module Gen
where

import Shape


--gOutcenters = BarycentricFunction (\a b c -> -a) (\a b c -> a)

--gOutradii = BarycentricFunction $ \a b c -> 2 * triangleAreaFromLengths(a,b,c) / (b+c-a)

--gInradius = BarycentricFunction $ \a b c -> 2 * triangleAreaFromLengths(a,b,c) / (a+b+c)
--gCircumradius = genRL (\a b c -> a*b*c / (4 * triangleAreaFromLengths(a,b,c)))
--gNinePointRadius = genRL (\a b c -> a*b*c / (8 * triangleAreaFromLengths(a,b,c)))

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------