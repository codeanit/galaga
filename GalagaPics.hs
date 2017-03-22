{-
  File      :  GalagaPics.hs
  Copyright : (c) E-Lynn Yap, 3/15/17
  Contains data structure to hold all constant pictures in game
-}

module GalagaPics
(
  GalagaPics(..),
  mkGalagaPics
) where

import Graphics.Gloss

data GalagaPics = GalagaPics {
                    menuPic :: Picture,
                    shipBulletPic :: Picture,
                    enemyBulletPic :: Picture,
                    shipNormalPic :: Picture,
                    shipExplosionPics :: [Picture],
                    shipProtectedPic :: Picture,
                    redEnemyPic :: Picture,
                    blueEnemyPic :: Picture,
                    enemyExplosionPics :: [Picture],
                    bossPartBeamPics :: [Picture],
                    bossFullBeamPics :: [Picture],
                    bossPics :: [Picture]
                  }

mkGalagaPics :: IO GalagaPics
mkGalagaPics = do 
  menuPic <- loadBMP "images/models/galaga.bmp"
  shipBulletPic <- loadBMP "images/models/ship_bullet.bmp"
  enemyBulletPic <- loadBMP "images/models/enemy_bullet.bmp"
  shipNormalPic <- loadBMP "images/models/ship.bmp"
  shipExplosionPics <- sequence [loadBMP "images/explosion/ship/frame1.bmp",
                        loadBMP "images/explosion/ship/frame2.bmp",
                        loadBMP "images/explosion/ship/frame3.bmp"]
  shipProtectedPic <- loadBMP "images/models/redship.bmp"
  redEnemyPic <- loadBMP "images/models/red_fighter.bmp"
  blueEnemyPic <- loadBMP "images/models/blue_fighter.bmp"
  enemyExplosionPics <- sequence [loadBMP "images/explosion/enemy/frame1.bmp",
                        loadBMP "images/explosion/enemy/frame2.bmp",
                        loadBMP "images/explosion/enemy/frame3.bmp",
                        loadBMP "images/explosion/enemy/frame4.bmp",
                        loadBMP "images/explosion/enemy/frame5.bmp"]
  bossPartBeamPics <- sequence [loadBMP "images/models/part_beam1.bmp",
                      loadBMP "images/models/part_beam2.bmp",
                      loadBMP "images/models/part_beam3.bmp",
                      loadBMP "images/models/part_beam4.bmp",
                      loadBMP "images/models/part_beam5.bmp",
                      loadBMP "images/models/part_beam6.bmp",
                      loadBMP "images/models/part_beam7.bmp",
                      loadBMP "images/models/part_beam8.bmp",
                      loadBMP "images/models/part_beam9.bmp"]
  bossFullBeamPics <- sequence [loadBMP "images/models/beam1.bmp",
                      loadBMP "images/models/beam2.bmp",
                      loadBMP "images/models/beam3.bmp"]
  bossPics <- sequence [loadBMP "images/models/boss_enemy1.bmp",
              loadBMP "images/models/boss_enemy2.bmp"]
  return $ GalagaPics menuPic shipBulletPic enemyBulletPic shipNormalPic
                      shipExplosionPics shipProtectedPic redEnemyPic
                      blueEnemyPic enemyExplosionPics bossPartBeamPics
                      bossFullBeamPics bossPics