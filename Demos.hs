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
mainDemo = demoNapoleonTriangle True

--------------------------------------------------------------------------------

demoCentroid :: Demo
demoCentroid t@(Triangle a b c) = 
    let c' = centroid t
    in do
            draw white t
            draw red $ t `ceviansThrough` c'
            draw red c'
    
demoSymmedian :: Demo
demoSymmedian t@(Triangle a b c) = 
    let c' = symmedianpoint t
    in do
            draw white t
            draw red $ t `ceviansThrough` c'
            draw red c'
    
demoOrthocentre :: Demo
demoOrthocentre t@(Triangle a b c) = 
    let c' = orthocentre t
    in do
            draw white t
            draw red $ t `ceviansThrough` c'
            draw red c'
    
demoMittenpunkt :: Demo
demoMittenpunkt t@(Triangle a b c) = 
    let mp = mittenpunkt t
    in do
            draw white t
            draw green $ outcircles t
            draw green $ outcentres t
            draw cyan $ rays mp (outcentres t)
            draw cyan $ mp

demoIncircle :: Demo
demoIncircle t@(Triangle a b c) = 
    let ic@(Circle c r) = incircle t
    in do
            draw white t
            draw yellow $ rays c (corners t)
            draw yellow $ c
            draw yellow $ ic

demoCircumcircle :: Demo
demoCircumcircle t@(Triangle a b c) = 
    let cc@(Circle c r) = circumcircle t
    in do
            draw white t
            draw cyan $ rays c (sidemidpoints t)
            draw green $ c
            draw green $ cc

demoNinePointCircle :: Demo
demoNinePointCircle t@(Triangle a b c) = 
    let o = orthocentre t
        heights = t `ceviansThrough` o
        hs = map (\(Line a b) -> b) heights
        npc@(Circle c r) = ninepointcircle t
    in do
            draw white $ t
            draw cyan $ heights
            draw blue $ raysMidpoints o (corners t)
            draw red $ sidemidpoints t
            draw yellow $ c
            draw yellow $ npc
            draw cyan $ hs

demoOuterNapoleonTriangle :: Demo
demoOuterNapoleonTriangle t =
    let ots = outerEquilateralTriangles t
    in do
            draw red $ ots
            draw green $ outerNapoleonTriangle t
            draw white $ t

-- isouter is True if you're using outer equilateral triangles, False for inner.
demoNapoleonTriangle :: Bool -> Demo
demoNapoleonTriangle isouter t =
    let ets = sideEquilateralTriangles isouter t
        nt = napoleonTriangle isouter t
    in do
            draw red $ ets
            draw green $ nt
            draw white $ t

demoNagel :: Demo
demoNagel t@(Triangle a b c) =
    let n = nagelpoint t
    in do
            draw white $ t
            draw cyan $ n
            draw cyan $ t `ceviansThrough` n
    
demoEulerLine :: Demo
demoEulerLine t@(Triangle a b c) =
    let o = orthocentre t
        hs = cevianIntersects t o
        cd = centroid t
        ms = cevianIntersects t cd
        cc@(Circle ccc ccr) = circumcircle t
        ex = exeterpoint t
        npc@(Circle npcc npcr) = ninepointcircle t
    in do
            draw (faded white) $ zipWith Line hs ms
            draw white $ t
            draw cyan $ t `ceviansThrough` o
            draw cyan $ rays o (corners t)
            draw cyan $ o
            draw green $ t `ceviansThrough` cd
            draw green $ cd
            draw blue $ ccc
            draw blue $ cc
            draw magenta $ ex
            draw yellow $ Line o ex
            draw yellow $ Line ccc ex
            draw yellow $ npcc
            draw yellow $ npc

--------------------------------------------------------------------------------
