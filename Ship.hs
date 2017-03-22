{-
  File      :  Ship.hs
  Copyright : (c) E-Lynn Yap, 3/3/17
  Ship module which contains the data type defining the ship in the game as
  well as methods to render and update it.
-}

module Ship 
(
   Ship,
   mkShip,
   render,
   moveLeft,
   moveRight,
   update,
   state,
   isActive,
   makeStationary,
   isDamagedBy
) where 

import Graphics.Gloss
import GameConstants
import Point
import Sprite
import GalagaPics

data Ship = Ship { loc :: Point, 
                   dim :: Size, 
                   state :: State,
                   explodingElapsed :: Float,
                   direction :: ShipDirection,
                   protectedElapsed :: Float }

-- directions that ship can move in
data ShipDirection = Left | Right | Stationary

-- Make ship an instance of Sprite
instance Sprite Ship where
  location = loc
  size = Ship.dim
  angle ship = 0
  getPic pics ship = case state ship of
    Alive -> shipNormalPic pics
    Protected -> shipProtectedPic pics
    Exploding n -> (shipExplosionPics pics) !! (n-1)
  isAlive ship = case state ship of
    Alive -> True
    Protected -> True
    _ -> False

-- For debugging
instance Show Ship where
  show ship = "Location: " ++ (show $ loc ship)


-- Creates a ship at the center of the bottom of the screen
mkShip :: Ship 
mkShip = Ship (0.0,-220.0) (30,30) Alive 0 Stationary 0

-- Changes a ship to stay stationary
makeStationary :: Ship -> Ship
makeStationary ship = ship { direction = Stationary }

-- Updates a ship so it keeps moving left unless key released
moveLeft :: Ship -> Ship
moveLeft ship = ship { direction = Ship.Left }

-- Updates a ship so it keeps moving right unless key released
moveRight :: Ship -> Ship
moveRight ship = ship { direction = Ship.Right }

-- Speed at which ship moves
shipSpeed :: Float
shipSpeed = 2

-- Whether the ship can move and fire bullets
isActive :: Ship -> Bool
isActive ship = case (Ship.state ship) of
                Alive -> True
                Protected -> True
                _ -> False

-- Whether the ship was damaged by any sprite from a list
isDamagedBy :: (Sprite a, Sprite b) => (a -> b -> Bool) -> a -> [b] -> Bool
isDamagedBy f ship foes = (any (== True) $ map (f ship) foes)

-- Updates a ship for exploding effect and horizontal movement
update :: Ship -> Float -> Ship
update (ship@Ship{state = Exploding n}) deltaTime
  | n == 3 = if advanceFrame then
                ship { state = Protected, explodingElapsed = 0, 
                       protectedElapsed = 0 } 
             else ship'
  | otherwise = if advanceFrame then
                  ship { state = Exploding (n+1), explodingElapsed = 0 } 
                else ship'
  where newElapsed = (explodingElapsed ship) + deltaTime
        advanceFrame = (newElapsed > 0.1)
        ship' = ship { explodingElapsed = newElapsed }
update (ship@Ship{direction = Ship.Left}) deltaTime
  | ((state ship) == Protected) && (elapsed' > 2) 
      = ship { state = Alive, protectedElapsed = 0, loc = newLoc }
  | (state ship) == Protected 
      = ship { protectedElapsed = elapsed', loc = newLoc }
  | otherwise = ship { loc = newLoc }
  where newLoc = updateX (loc ship) movement
        minX = -(fromIntegral $ (screenWidth `div` 2) - (fst $ size ship))
        movement = if ((getX $ loc ship) - distance) < minX
                    then 0 else (-distance)
        distance = deltaTime * shipSpeed * (fromIntegral fps)
        elapsed' = (protectedElapsed ship) + deltaTime
update (ship@Ship{direction = Ship.Right}) deltaTime 
  | ((state ship) == Protected) && (elapsed' > 3) 
      = ship { state = Alive, protectedElapsed = 0, loc = newLoc }
  | (state ship) == Protected 
      = ship { protectedElapsed = elapsed', loc = newLoc }
  | otherwise = ship { loc = newLoc }
  where newLoc = updateX (loc ship) movement
        maxX = fromIntegral $ (screenWidth `div` 2) - (fst $ size ship)
        movement = if ((getX $ loc ship) + distance) > maxX
                    then 0 else distance
        distance = deltaTime * shipSpeed * (fromIntegral fps) 
        elapsed' = (protectedElapsed ship) + deltaTime
update (ship@Ship{direction = Ship.Stationary}) deltaTime 
  | ((state ship) == Protected) && (elapsed' > 3) 
      = ship { state = Alive, protectedElapsed = 0 }
  | (state ship) == Protected 
      = ship { protectedElapsed = elapsed' }
  | otherwise = ship
  where elapsed' = (protectedElapsed ship) + deltaTime