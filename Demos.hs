--------------------------------------------------------------------------------
-- Contains predefined demos, which are functions that take in a Triangle,
-- and draw some of its properties using the Draw module.
-- (the list of properties and colours is different for each demo)
--------------------------------------------------------------------------------

module Demos
( mainDemo
) where

import Draw
import Centres
import Shape
import Constructions

--------------------------------------------------------------------------------

type Demo = Triangle -> IO ()

mainDemo :: Demo
mainDemo = demoCentroid

--------------------------------------------------------------------------------

demoCentroid :: Demo
demoCentroid t@(Triangle a b c) = 
    let c' = centroid t
    in do
            draw white t
            draw red $ t `ceviansThrough` c'
            draw red c'
    
--demoNapoleonTriangleOuter :: Triangle -> IO ()
--demoNapoleonTriangleOuter t = do
--    draw colourRed (triangles outerEquilateralTriangles) t
--    draw colourGreen (triangle napoleonTriangle) t
--    draw colourWhite id t

--demoMittenpunkt :: Demo
--demoMittenpunkt t@(Triangle a b c) = 
--    let c' = centroid t
--    in do
--            draw white t
--            draw red $ t `ceviansThrough` gCentroid
--            draw red centroid
--            draw green (points gOutcenters) t
--            draw colourGreen (circles gOutcenters gOutradii) t
--            draw colourCyan (rays gMittenpunkt gOutcenters) t
--            draw colourCyan (point gMittenpunkt) t

-- This is how I want it to be:
-- 
-- demoCentroid t@(Triangle a b c) = 
--     let c = centroid t
--         ms = t `ceviansThrough` c
--      in do
--           drawTriangle white t
--           drawPoint red c
--           drawLines red ms

--demoIncircle t = do
--    draw colourWhite id t
--    draw colourYellow (rays gIncenter gId) t
--    draw colourYellow (point gIncenter) t
--    draw colourYellow (circle gIncenter gInradius) t
--
--demoCircumcircle t = do
--    draw colourWhite id t
--    draw colourCyan (rays gCircumcenter (gCevianIntersects gCentroid)) t
--    draw colourGreen (circle gCircumcenter gCircumradius) t
--
--demoOrthocenter t = do
--    draw (colourFaded colourWhite)
--      (lineSegments (gCevianIntersects gOrthocenter) (gCevianIntersects gCentroid))
--      t
--    draw colourWhite id t
--    -- Need both cevians and rays in case the point is outside the triangle
--    draw (colourFaded colourCyan) (rays gOrthocenter gId) t
--    draw colourCyan (cevians gOrthocenter) t
--    draw colourCyan (point gOrthocenter) t

--demoSymmedian t = do
--    draw colourWhite id t
--    draw colourCyan (cevians gSymmedian) t
--    draw colourCyan (point gSymmedian) t
--
--demoNagel t = do
--    draw colourWhite id t
--    draw colourCyan (cevians gNagel) t
--    draw colourCyan (point gNagel) t
    
--demoEulerLine t = do
--    draw (colourFaded colourWhite)
--      (lineSegments (gCevianIntersects gOrthocenter) (gCevianIntersects gCentroid))
--      t
--    draw colourWhite id t
--    draw colourCyan (cevians gOrthocenter) t
--    draw colourCyan (rays gOrthocenter gId) t
--    draw colourCyan (point gOrthocenter) t
--    draw colourGreen (cevians gCentroid) t
--    draw colourGreen (point gCentroid) t
--    draw colourBlue (point gCircumcenter) t
--    draw colourBlue (circle gCircumcenter gCircumradius) t
--    draw colourMagenta (point gExeter) t
--    draw colourYellow (lineSegment gOrthocenter gExeter) t
--    draw colourYellow (lineSegment gCircumcenter gExeter) t
--    draw colourYellow (point gNinePointCenter) t
--    draw colourYellow (circle gNinePointCenter gNinePointRadius) t

--demoNinePointCircle t = do
--    draw colourRed (lineSegments (gCevianIntersects gOrthocenter) (gCevianIntersects gCentroid)) t
--    draw colourWhite id t
--    draw colourCyan (cevians gOrthocenter) t
--    draw colourCyan (rays gOrthocenter gId) t
--    draw colourCyan (points (gCevianIntersects gOrthocenter)) t
--    draw colourGreen (points (gCevianIntersects gCentroid)) t
--    draw colourBlue (raysMidpoints gOrthocenter gId) t
--    draw colourYellow (point gNinePointCenter) t
--    draw colourYellow (circle gNinePointCenter gNinePointRadius) t

--------------------------------------------------------------------------------
