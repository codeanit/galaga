{-
  File      :  Bullet.hs
  Copyright : (c) E-Lynn Yap, 3/3/17
  Bullet module which contains the data type defining the enemy and ship
  bullets in the game as well as methods to render and update them.
-}

module Bullet
(
   Bullet,
   addEnemyBullet,
   addShipBullet,
   bulletType,
   isEnemyBullet,
   isShipBullet,
   removeDestroyed,
   render,
   update
) where

import Graphics.Gloss
import GameConstants
import Point
import Sprite
import AngledTrajectory
import GalagaPics

data Bullet = Bullet { loc :: Point, 
                       dim :: Size, 
                       bulletType :: BulletType,
                       rotation :: Float }

-- Bullets can be either ship bullets or enemy bullets
data BulletType = ShipBullet | EnemyBullet deriving Eq

-- Make Bullet an instance of Sprite
instance Sprite Bullet where
  getPic pics bullet 
    | bulletType bullet == ShipBullet = Rotate x $ shipBulletPic pics
    | bulletType bullet == EnemyBullet = Rotate x $ enemyBulletPic pics
    where x = angle bullet
  location = loc
  size = Bullet.dim
  angle = rotation
  isAlive bullet = True

-- Speed of a bullet
bulletSpeed :: Float
bulletSpeed = 10

-- Adds a ship's bullet to a list of bullets based on ship's location
addShipBullet :: Sprite a => a -> [Bullet] -> [Bullet]
addShipBullet ship bullets = newBullet:bullets
  where newBullet = mkBullet ship ShipBullet

-- Adds an enemy's bullet to a list of bullets based on enemy's location
addEnemyBullet :: Sprite a => a -> [Bullet] -> [Bullet]
addEnemyBullet enemy bullets = newBullet:bullets
  where newBullet = mkBullet enemy EnemyBullet

-- Whether a bullet is a ship's bullet
isShipBullet :: Bullet -> Bool
isShipBullet bullet = (bulletType bullet == ShipBullet)

-- Whether a bullet is an enemy's bullet
isEnemyBullet :: Bullet -> Bool
isEnemyBullet bullet = (bulletType bullet == EnemyBullet)

-- Creates a new bullet based on the sprite's location and type (ship/enemy)
mkBullet :: Sprite a => a -> BulletType -> Bullet
mkBullet sprite bulletType
  | bulletType == ShipBullet = let y = spriteY + spriteHeight
                               in 
                               Bullet (spriteX, y) bulletSize ShipBullet angle'
  | bulletType == EnemyBullet = let y = spriteY - spriteHeight
                                in 
                                Bullet (spriteX, y) bulletSize EnemyBullet angle'
  where spriteX = getX $ location sprite
        spriteY = getY $ location sprite
        spriteHeight = fromIntegral (snd $ size sprite)
        bulletSize = (10, 17)
        angle' = angle sprite

-- Updates the bullet to move up/down depending on their type
update :: Float -> Bullet -> Bullet
update deltaTime bullet
  | bulletType bullet == ShipBullet = bullet { 
                                      loc = updateY (loc bullet) distance }
  | bulletType bullet == EnemyBullet = let speed' = (bulletSpeed / 2)
                                       in bullet { loc = newLoc speed' }
  where angle' = angle bullet
        currLoc = loc bullet
        newLoc speed = getNewCoords angle' speed deltaTime currLoc
        distance = deltaTime * (fromIntegral fps) * bulletSpeed

-- Takes a list of bullets, enemies, bosses and the ship and returns 
-- a new list of bullets excluding the ones that were destroyed
removeDestroyed :: (Sprite a, Sprite b, Sprite c) => 
                   [Bullet] -> [a] -> [b] -> c -> [Bullet]
removeDestroyed bullets enemies bosses ship = filter unDestroyed bullets
  where unDestroyed bullet = not $ isDestroyed bullet enemies bosses ship

-- Checks if a bullet was destroyed based on its type and any collisions
isDestroyed :: (Sprite a, Sprite b, Sprite c) => 
               Bullet -> [a] -> [b] -> c -> Bool
isDestroyed bullet enemies bosses ship = case bulletType bullet of
  -- if it's a ship bullet, check if it collided with an enemy
  ShipBullet -> (any (== True) $ map (detectCollision bullet) enemies) ||
                (any (== True) $ map (detectCollision bullet) bosses)
  -- if it's an enemy bullet, check if it collided with the ship
  EnemyBullet -> detectCollision bullet ship