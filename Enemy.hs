{-
  File      :  Enemy.hs
  Copyright : (c) E-Lynn Yap, 3/3/17
  Enemy module which contains the data type defining enemies in the game as
  well as methods to render and update them.
-}

module Enemy
(
   Enemy(..),
   EnemyDirection(..),
   EnemyType(..),
   advanceEnemy,
   enemyDimensions,
   enemySpeed,
   isAdvancing,
   render,
   update
) where

import Graphics.Gloss
import GameConstants
import Point
import Sprite
import GalagaPics
import qualified Bezier

-- Defines an enemy in the game
data Enemy = Enemy { loc :: Point,
                     dim :: Size, 
                     direction :: EnemyDirection, 
                     fighterType :: EnemyType, 
                     speed :: Float,
                     state :: State, 
                     explodingElapsed :: Float,
                     rotation :: Float
                    } deriving Show

-- Make Enemy an instance of Sprite
instance Sprite Enemy where
  getPic pics enemy = case (state enemy, direction enemy) of
    (Alive, Curve (bPoints, orientation)) -> case (fighterType enemy) of
      -- rotate the enemy as it moves along a bezier curve
      Red -> Rotate x $ redEnemyPic pics
      Blue -> Rotate x $ blueEnemyPic pics
      where x = angle enemy
    (Alive, _) -> case (fighterType enemy) of
      Red -> redEnemyPic pics
      Blue -> blueEnemyPic pics
    (Exploding n, _) -> (enemyExplosionPics pics) !! (n-1)
  location = loc
  size = Enemy.dim
  angle = rotation
  isAlive enemy = (state enemy) == Alive

-- Directions that enemy can move in
data EnemyDirection = Left | Right | Down | Curve ([Point],Bezier.Orientation)
                      deriving (Eq, Show)

-- Various types of enemies
data EnemyType = Red | Blue deriving (Eq, Show)

-- Dimensions of an enemy
enemyDimensions :: (Int, Int)
enemyDimensions = (30, 24)

-- The speed at which the enemy initially moves
enemySpeed :: Float
enemySpeed = 1

-- Updates an enemy's location based on its current direction and time elapsed
-- and whether it is currently alive/exploding
update :: EnemyDirection -> Float -> Enemy -> Enemy
update dir deltaTime enemy@Enemy{ state = (Exploding n) }
  | n == 5 = if advanceFrame 
             then enemy { state = Dead, explodingElapsed = 0 }
             else enemy'
  | otherwise = if advanceFrame 
                then enemy { state = Exploding (n+1), explodingElapsed = 0 }
                else enemy'
  where enemy' = enemy { explodingElapsed = newElapsed }
        newElapsed = (explodingElapsed enemy) + deltaTime
        advanceFrame = newElapsed > 0.10
update _ deltaTime enemy@Enemy{ direction = Curve (bPoints, orientation) }
  | step <= (length bPoints) = enemy { direction = Curve ((drop step bPoints),
                                                          orientation),
                                       loc = bPoints !! (step - 1),
                                       rotation = newAngle
                                    }
  | otherwise = let distance = deltaTime * (speed enemy) * (fromIntegral fps)
                    newLoc = updateY (loc enemy) (-distance)
                in enemy { direction = Down, loc = newLoc, rotation = newAngle }
  where step = ceiling $ deltaTime * (fromIntegral fps)
        newAngle = if orientation == Bezier.Right then
                     if l > 90 then 345 else 15
                   else
                     if l > 90 then 15 else 345
        l = fromIntegral $ length bPoints -- where it is along the b curve
update dir deltaTime enemy = enemy { loc = newLoc, direction = newDirection }
  where newLoc = case newDirection of
          Enemy.Left -> updateX currLoc (-distance)
          Enemy.Right -> updateX currLoc distance
          Enemy.Down -> updateY currLoc (-distance)
        newDirection = case currDirection of
          Down -> Down -- don't change direction if alr advancing
          _ -> dir -- follow the direction of the formation
        currDirection = direction enemy
        currLoc = loc enemy
        distance = deltaTime * (speed enemy) * (fromIntegral fps)

-- Makes an enemy currently in formation start advancing
-- When an enemy starts advancing it moves in a bezier curve
advanceEnemy :: Enemy -> Enemy
advanceEnemy enemy = enemy {direction = Curve bezier }
  where bezier = (Bezier.computeVerticalBezPath $ loc enemy)

-- Determines if enemy is advancing
isAdvancing :: Enemy -> Bool
isAdvancing Enemy{ direction = Down } = True
isAdvancing Enemy{ direction = Curve _ } = True
isAdvancing _ = False