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
import Geo

--------------------------------------------------------------------------------
-- Demo type declaration

type Demo = Triangle -> IO ()

--------------------------------------------------------------------------------
-- The demo that is used by main program

mainDemo :: Demo
mainDemo = demoNinePointCircle

--------------------------------------------------------------------------------
-- List of demos

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

demoIncircle t@(Triangle a b c) = 
    let circ@(Circle c' r') = incircle t
    in do
        draw white t
        draw yellow $ rays c' t
        draw yellow c'
        draw yellow circ

demoCircumcircle t = 
    let circ@(Circle c' r') = circumcircle t
    in do
        draw white t
        draw cyan $ rays c' (midpoints t)
        draw green circ

demoOrthocentre t@(Triangle a b c) = 
    let o = orthocentre t
        c = centroid t
        [ma,mb,mc] = midpoints t
        [x,y,z] = heightIntersects t
        hs = heights t
    in do
        -- extend sides all the way to height-intersects
        draw red $ Line x ma
        draw red $ Line y mb
        draw red $ Line z mc
        -- draw triangle on top of the above extensions
        draw white t
        -- Draw the heights
        draw cyan $ hs
        -- In case the point is outside the triangle
        draw cyan $ rays o t
        -- Orthocentre
        draw cyan o

demoSymmedian t = 
    let s = symmedianpoint t
    in do
        draw white t
        draw cyan $ rays s t
        draw cyan s

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

demoNinePointCircle t =
    let o = orthocentre t
        c = centroid t
        [ma,mb,mc] = midpoints t
        [x,y,z] = heightIntersects t
    in do
        -- Orthocentre
        draw red $ Line x ma
        draw red $ Line y mb
        draw red $ Line z mc
        draw white t
        draw cyan $ heights t
        draw cyan $ rays o t
        draw cyan o
        draw green (midpoints t)
        draw blue (map lineMidpoint $ rays o t)
        draw yellow (ninepointcentre t)
        draw yellow (ninepointcircle t)

--------------------------------------------------------------------------------
