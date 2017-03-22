{-
  File      :  Bezier.hs
  Copyright : (c) E-Lynn Yap, 3/10/17
  Methods for computing Bezier curves for movement of enemies.
-}

module Bezier
(
  computeVerticalBezPath,
  computeHorizontalBezPath,
  Orientation(..)
) where

import Graphics.Gloss
import Point

-- Whether it curves to the left or right
data Orientation = Left | Right deriving (Eq, Show)

-- Number of line segments in a curve
numOfSegments :: Int
numOfSegments = 180

-- Calculates a Bezier point on the path (quadratic curve)
getBezierPoint :: Point -> Point -> Point -> Float -> Point
getBezierPoint p0 p1 p2 t = firstTerm + secondTerm + thirdTerm
  where firstTerm = scalePoint p0 (a * a)
        secondTerm = scalePoint p1 (2 * a * t)
        thirdTerm = scalePoint p2 (t * t)
        a = (1 - t)

-- Takes in the starting point and computes a quadratic Bezier curve
-- from that point to another point vertically below it
-- Path is in the form of a list of points
computeVerticalBezPath :: Point -> ([Point], Orientation)
computeVerticalBezPath p0@(x0,y0) = (bPoints, orientation)
  where bPoints = map (getBezierPoint p0 p1 p2 . f) [0..numOfSegments]
        f segmentNum = (fromIntegral segmentNum) / (fromIntegral numOfSegments)
        p2 = (x0, y0-300)
        p1 = (x1, y0-150)
        -- ensure that sprite doesn't move offscreen
        x1 = if x0 < 0 then x0 + 250 else x0 - 250
        orientation = if x0 < 0 then Bezier.Right else Bezier.Left

-- Takes in the starting point and computes a quadratic Bezier curve
-- from that point to another point horizontally to the left of it
computeHorizontalBezPath :: Point -> [Point]
computeHorizontalBezPath p0@(x0,y0) = bPoints
  where bPoints = map (getBezierPoint p0 p1 p2 . f) [0..numOfSegments]
        f segmentNum = (fromIntegral segmentNum) / (fromIntegral numOfSegments)
        p2 = (x0-300, y0)
        p1 = (x0-150, y0+150)
