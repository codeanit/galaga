{- 
   File      :  Sprite.hs 
   Represents the Sprite typeclass, which includes definitions for rendering 
   and updating the game 
-}
module Sprite 
(
   Sprite,
   State(..),
   angle,
   detectCollision,
   eliminateOffScreen,
   getPic,
   isAlive,
   location,
   render,
   size,
   xCoord,
   yCoord
) where  

import Graphics.Gloss
import GameConstants
import Point
import GalagaPics

data State = Alive | Exploding Int | Dead | Protected deriving (Show, Eq)

class Sprite a where 
  location :: a -> Point
  size :: a -> Size
  angle :: a -> Float
  isAlive :: a -> Bool
  getPic :: GalagaPics -> a -> Picture
  -- Renders the sprite based on its pic and location
  render :: GalagaPics -> a -> IO Picture
  render pics a = return $ Translate (xCoord a) (yCoord a) (getPic pics a)
  -- Retrieves x coordinate of sprite
  xCoord :: a -> Float
  xCoord a = getX $ location a
  -- Retrieves y coordinate of sprite
  yCoord :: a -> Float
  yCoord a = getY $ location a
  -- Checks if sprite has moved off screen
  isOffScreen :: a -> Bool
  isOffScreen a = x < minX || x > maxX || y < minY || y > maxY
    where x = xCoord a
          y = yCoord a
          width = fromIntegral screenWidth
          height = fromIntegral screenHeight
          minX = -(width/2)
          maxX = width/2
          minY = -(height/2)
          maxY = height/2
  -- Eliminates offscreen sprites from a list
  eliminateOffScreen :: [a] -> [a]
  eliminateOffScreen = filter (not . isOffScreen)

-- Checks if two sprites have collided
detectCollision :: (Sprite a, Sprite b) => a -> b -> Bool
detectCollision sprite1 sprite2 = bothAlive && intersectX && intersectY
  where bothAlive = (isAlive sprite1) && (isAlive sprite2)
        intersectX = (abs ((xCoord sprite1) - (xCoord sprite2)) * 2) <
                      fromIntegral (((fst $ size sprite1) + 
                                      (fst $ size sprite2)))
        intersectY = (abs ((yCoord sprite1) - (yCoord sprite2)) * 2) <
                      fromIntegral (((snd $ size sprite1) + 
                                        (snd $ size sprite2)))
