{-
  File      :  EnemyController.hs
  Copyright : (c) E-Lynn Yap, 3/3/17
  Encapsulates a formation of enemies on screen.
-}

module EnemyController
(
  EnemyController,
  advance,
  allKilled,
  clearOffScreen,
  expandFormation,
  getEnemies,
  markAndCountDestroyed,
  mkEnemies,
  EnemyController.render,
  removeDead,
  EnemyController.update
) where

import Graphics.Gloss
import GameConstants
import Point
import Enemy
import Sprite
import GalagaPics

-- Defines a collection of enemies
data EnemyController = EnemyController {
                          getEnemies :: [Enemy],
                          moveElapsed :: Float,
                          direction :: EnemyDirection,
                          topY :: Float, -- y coord of topmost enemy
                          bottomY :: Float,
                          leftX :: Float, -- x coord of leftmost enemy
                          rightX :: Float,
                          speed :: Float
                        }

-- Creates the initial formation of enemies at start of game
mkEnemies :: Int -> EnemyController
mkEnemies enemiesInRow = EnemyController enemies 0 Enemy.Left 
                         topY bottomY leftX rightX enemySpeed
  where enemies = (initEnemies (leftX) topY enemiesInRow Blue) ++ 
                  (initEnemies (leftX) (topY-spacing) enemiesInRow Red) ++
                  (initEnemies (leftX) bottomY enemiesInRow Blue)
        topY = 200
        leftX = (-50)
        rightX = leftX + fromIntegral (enemiesInRow - 1) * spacing
        bottomY = topY-2*spacing

-- Renders the enemies on the screen
render :: EnemyController -> GalagaPics -> IO [Picture]
render controller pics = mapM (Enemy.render pics) (getEnemies controller)

-- Spacing between enemies
spacing :: Num a => a
spacing = 40

-- Creates a specified number of enemies in a row starting from given (x,y)
initEnemies :: Float -> Float -> Int -> EnemyType -> [Enemy]
initEnemies x y count enemyType = map f (take count [x,x+spacing..])
  where f x' = Enemy (x', y) enemyDimensions Enemy.Left enemyType enemySpeed
               Alive 0 0

-- Updates the enemies based on elapsed time
update :: Float -> EnemyController -> EnemyController
update deltaTime controller
  | moveElapsed' > 2 = controller { getEnemies = map (Enemy.update oppDirection 
                        deltaTime) enemies', moveElapsed = 0, 
                        EnemyController.direction = oppDirection,
                        leftX = newLeftX oppDirection,
                        rightX = newRightX oppDirection }
  | otherwise = controller { getEnemies = map (Enemy.update direction' 
                                            deltaTime) enemies',
                            moveElapsed = moveElapsed',
                            EnemyController.direction = direction',
                            leftX = newLeftX direction',
                            rightX = newRightX direction' }
  where direction' = EnemyController.direction controller
        oppDirection = case direction' of
          Enemy.Left -> Enemy.Right
          Enemy.Right -> Enemy.Left
        moveElapsed' = moveElapsed controller + deltaTime
        enemies' = getEnemies controller
        newLeftX dir = case dir of
          Enemy.Left -> left - distance
          Enemy.Right -> left + distance
          where left = leftX controller
        newRightX dir = case dir of
          Enemy.Left -> right - distance
          Enemy.Right -> right + distance
          where right = rightX controller
        distance = deltaTime * (EnemyController.speed controller) * 
                    (fromIntegral fps)

-- Takes a list of enemies, ship bullets, and the ship and marks those
-- enemies that have been damaged. Returns new controller + num destroyed
markAndCountDestroyed :: (Sprite a, Sprite b) => EnemyController -> [a] -> b 
                          -> (EnemyController, Int)
markAndCountDestroyed controller bullets ship = 
  let enemies' = map changeStateToExploding $ getEnemies controller
      numDestroyed = length $ filter isDestroyed $ getEnemies controller
      changeStateToExploding enemy = if (isDestroyed enemy) then
                                        enemy { state = Exploding 1 }
                                     else enemy
      isDestroyed enemy = (state enemy == Alive ) && ((collidedWithShip enemy) 
                          || (collidedWithBullet enemy))
      collidedWithShip enemy = detectCollision enemy ship
      collidedWithBullet enemy = any (== True)
                                  $ map (detectCollision enemy) bullets
  in
  (controller { getEnemies = enemies' }, numDestroyed)

-- Returns true if all onscreen enemies have been killed
allKilled :: EnemyController -> Bool
allKilled = null . getEnemies

-- Starts advancing a specified number of enemies
advance :: Int -> EnemyController -> EnemyController
advance n controller = controller { getEnemies = enemies' }
  where enemies' = fst $ foldr f ([], 0) $ getEnemies controller
        f enemy (enemies', count) 
          = if (count < n) && (isInFormation enemy) && (isAlive enemy)
            then ((advanceEnemy enemy):enemies', count + 1)
            else (enemy:enemies', count)
        isInFormation enemy = (Enemy.direction enemy /= Down)
        isAlive enemy = (state enemy) == Alive

-- Removes the enemies that have moved offscreen
clearOffScreen :: EnemyController -> EnemyController
clearOffScreen controller = controller { getEnemies = eliminateOffScreen enemies' }
  where enemies' = getEnemies controller

-- Adds a specified num of enemies to the formation
expandFormation :: Int -> EnemyController -> EnemyController
expandFormation n controller = controller { getEnemies = enemies' ++ newEnemies }
  where enemies' = getEnemies controller
        newEnemies = (map (\loc -> Enemy loc enemyDimensions dir Red 
                          enemySpeed Alive 0 0) newLocsRed) ++
                      (map (\loc -> Enemy loc enemyDimensions dir Blue 
                          enemySpeed Alive 0 0) newLocsBlue)
        dir = EnemyController.direction controller
        (newLocsBlue, newLocsRed) = splitHalf newLocs
        newLocs = take n emptySpots
        emptySpots = filter (\loc -> not $ loc `elem` filledSpots) allSpots
        filledSpots = map loc enemies'
        allSpots = [ (x,y) | x <- [leftX controller, leftX controller + spacing
                                  .. rightX controller],
                             y <- [topY controller, topY controller - spacing,
                                    bottomY controller]]
        splitHalf l = splitAt ((length l + 1) `div` 2) l

-- Removes the dead enemies from the controller
removeDead :: EnemyController -> EnemyController
removeDead controller = controller { getEnemies = enemies' }
  where enemies' = filter (not . isDead) (getEnemies controller)
        isDead enemy = (state enemy) == Dead
