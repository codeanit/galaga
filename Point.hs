{-
  File      :  Point.hs
  Copyright : (c) E-Lynn Yap, 3/3/17
  Point module which contains methods to make accessing and updating Points
  easier.
-}

module Point
(
  getX,
  getY,
  updateX,
  updateY,
  scalePoint
) where

import Graphics.Gloss.Data.Point

-- Retrieves x coord from Point
getX :: Point -> Float
getX (x, _) = x

-- Retrieves y coord from Point
getY :: Point -> Float
getY (_, y) = y

-- Updates the x coord of a point by adding a specified number
updateX :: Point -> Float -> Point
updateX (x, y) num = (x + num, y)

-- Updates the y coord of a point by adding a specified number
updateY :: Point -> Float -> Point
updateY (x, y) num = (x, y + num)

-- Performs scalar multiplication
scalePoint :: Point -> Float -> Point
scalePoint (x, y) n = (n*x, n*y)