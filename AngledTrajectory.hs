{-
  File      :  AngledTrajectory.hs
  Copyright : (c) E-Lynn Yap, 3/3/17
  Contains trigonometric functions to calculate the movement for a sprite
  that's moving diagonally at an angle.
-}

module AngledTrajectory
(
  getNewCoords
) where

import Graphics.Gloss.Data.Point
import Point
import GameConstants

-- Computes the next (x,y) coordinate for a sprite that's rotated at n
-- degrees (clockwise around the y-axis) at a given speed
getNewCoords :: Float -> Float -> Float -> Point -> Point
getNewCoords angle speed deltaTime (x1,y1)
  | (angle > 180) && (angle <= 360) = (x1 + xOffset, y1 - yOffset)
  | otherwise = (x1 - xOffset, y1 - yOffset)
  where xOffset = abs $ (sin angle') * distance
        yOffset = abs $ (cos angle') * distance
        distance = deltaTime * (fromIntegral fps) * speed
        angle' = if (angle >= 270 && angle <= 360) 
                 then (360 - angle)
                 else angle