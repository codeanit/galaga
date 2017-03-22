{-
  File      :  Boss.hs
  Copyright : (c) E-Lynn Yap, 3/3/17
  Boss module which contains the data type defining the boss-level enemy
  in the game as well as methods to render and update it.
-}

module Boss
(
  Boss(..),
  mkBoss,
  update,
  render,
  markAndCountDestroyed,
  removeDead,
  isCapturedByBeam
) where 

import Graphics.Gloss
import GameConstants
import Point
import Sprite
import qualified Bezier
import GalagaPics

-- Defines an enemy boss
data Boss = Boss { loc :: Point,
                   dim :: Size,
                   direction :: BossDirection,
                   speed :: Float,
                   state :: State,
                   explodingElapsed :: Float,
                   moveElapsed :: Float,
                   pincerElapsed :: Float,
                   pincerIsOpen :: Bool,
                   beamElapsed :: Float
                  }

-- Make an instance of Sprite
instance Sprite Boss where
  getPic pics boss = case state boss of
    Alive ->  case direction boss of
      Stationary n -> if n > 9 then
                        let beamPic = if (n `mod` 3 == 0) then
                                        Translate 0 (-95) $
                                        (bossFullBeamPics pics) !! 0
                                      else if (n `mod` 3 == 1) then
                                        Translate 0 (-95) $ 
                                        (bossFullBeamPics pics) !! 1
                                      else Translate 0 (-95) $ 
                                        (bossFullBeamPics pics) !! 2
                        in Pictures [beamPic, bossPic]
                      else
                        let y = 7.5 * (fromIntegral n) + 20
                            beamPic = Translate 0 (-y) $ 
                                    (bossPartBeamPics pics) !! (n-1)
                        in Pictures [beamPic, bossPic]
      _ -> if (pincerIsOpen boss) then ((bossPics pics) !! 0)
           else ((bossPics pics) !! 1)
    Exploding n -> (enemyExplosionPics pics) !! (n-1)
    where bossPic = (bossPics pics) !! 0
  location = loc
  size = Boss.dim
  angle boss = 0
  isAlive boss = (state boss) == Alive

-- Directions that boss can move in
data BossDirection = Left | Right | Curve [Point] | Stationary Int
                     deriving (Eq, Show)

-- Dimensions of a boss
bossDimensions :: (Int, Int)
bossDimensions = (35, 35)

-- Dimensions of the beam
beamDimensions :: (Int, Int)
beamDimensions = (86, 150)

-- Speed at which it moves
bossSpeed :: Float
bossSpeed = 1.5

-- Creates a boss
mkBoss :: Boss
mkBoss = Boss loc bossDimensions (Curve bPoints) bossSpeed Alive 0 0 0 True 0
  where loc = (fromIntegral screenWidth/2, -40)
        bPoints = Bezier.computeHorizontalBezPath loc

-- Updates a boss' location
update :: Float -> Boss -> Boss
update deltaTime boss@Boss{ state = (Exploding n) }
  | n == 5 = if advanceFrame 
             then boss { state = Dead, explodingElapsed = 0 }
             else boss'
  | otherwise = if advanceFrame 
                then boss { state = Exploding (n+1), explodingElapsed = 0 }
                else boss'
  where boss' = boss { explodingElapsed = newElapsed }
        newElapsed = (explodingElapsed boss) + deltaTime
        advanceFrame = newElapsed > 0.10
update deltaTime boss@Boss{ direction = Curve bPoints }
  | step <= (length bPoints) = boss { direction = Curve (drop step bPoints),
                                      loc = bPoints !! (step - 1),
                                      pincerElapsed = pincerElapsed',
                                      pincerIsOpen = pincerIsOpen' }
  | otherwise = let distance = deltaTime * (speed boss) * (fromIntegral fps)
                    newLoc = updateX (loc boss) distance
                in boss { direction = Boss.Left, loc = newLoc,
                          pincerElapsed = pincerElapsed',
                          pincerIsOpen = pincerIsOpen' }
  where step = ceiling $ deltaTime * (fromIntegral fps)
        (pincerElapsed', pincerIsOpen') = updatePincer boss deltaTime
update deltaTime boss@Boss { direction = Stationary n }
  | (beamElapsed' > 0.15) && (n == 50) = let newDir = if (xCoord boss < 0)
                                                      then Boss.Right
                                                      else Boss.Left
                                          in boss { direction = newDir,
                                                    beamElapsed = 0 }
  | (beamElapsed' > 0.15) = boss { direction = Stationary (n+1),
                                   beamElapsed = 0 }
  | otherwise = boss { beamElapsed = beamElapsed' }
  where beamElapsed' = (beamElapsed boss) + deltaTime
update deltaTime boss
  | beamElapsed' > 5 = boss { direction = Stationary 1, 
                              beamElapsed = 0,
                              moveElapsed = 0 }
  | moveElapsed' > 3 = boss { direction = oppDir, moveElapsed = 0,
                              loc = newLoc oppDir, 
                              pincerElapsed = pincerElapsed',
                              pincerIsOpen = pincerIsOpen',
                              beamElapsed = beamElapsed' }
  | otherwise = boss { moveElapsed = moveElapsed', loc = newLoc currDir,
                       pincerElapsed = pincerElapsed',
                       pincerIsOpen = pincerIsOpen',
                       beamElapsed = beamElapsed' }
  where moveElapsed' = (moveElapsed boss) + deltaTime
        currDir = direction boss
        oppDir = case currDir of
          Boss.Left -> Boss.Right
          Boss.Right -> Boss.Left
        newLoc dir = case dir of
          Boss.Left -> updateX currLoc (-distance)
          Boss.Right -> updateX currLoc distance
        currLoc = loc boss
        distance = deltaTime * (fromIntegral fps) * (speed boss)
        (pincerElapsed', pincerIsOpen') = updatePincer boss deltaTime
        beamElapsed' = (beamElapsed boss) + deltaTime

-- Checks elapsed time to determine if pincer is open or closed
updatePincer :: Boss -> Float -> (Float, Bool)
updatePincer boss deltaTime = (pincerElapsed', pincerIsOpen')
  where pincerElapsed' = if newElapsed > 0.2 then 0 else newElapsed
        pincerIsOpen' = if newElapsed > 0.2 then not (pincerIsOpen boss)
                       else (pincerIsOpen boss)
        newElapsed = (pincerElapsed boss) + deltaTime

-- Marks and counts those bosses that have been destroyed
markAndCountDestroyed :: (Sprite a, Sprite b) => [Boss] -> [a] -> b 
                          -> ([Boss], Int)
markAndCountDestroyed boss bullets ship = 
  let boss' = map changeStateToExploding boss
      numDestroyed = length $ filter isDestroyed boss
      changeStateToExploding boss = if (isDestroyed boss) then
                                        boss { state = Exploding 1 }
                                     else boss
      isDestroyed boss = (state boss == Alive ) && ((collidedWithShip boss) 
                          || (collidedWithBullet boss))
      collidedWithShip boss = detectCollision boss ship
      collidedWithBullet boss = any (== True)
                                  $ map (detectCollision boss) bullets
  in
  (boss', numDestroyed)

-- Removes bosses that are dead
removeDead :: [Boss] -> [Boss]
removeDead boss = filter (\b -> (state b) /= Dead) boss

-- Check if ship has been captured by beam
isCapturedByBeam :: (Sprite a) => a -> Boss -> Bool
isCapturedByBeam ship boss@Boss{ direction = Stationary n }
  | n < 10 = False -- can only be captured when beam fully extended
  | otherwise = (abs ((xCoord boss) - (xCoord ship)) * 2) <
                fromIntegral (((fst beamDimensions) + (fst $ size ship)))
isCapturedByBeam _ _ = False
