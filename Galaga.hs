{-
  File      :  Galaga.hs
  Copyright : (c) E-Lynn Yap, 3/10/17
  Main module for Galaga game using Gloss library.
-}

module Main where

import Ship
import EnemyController
import qualified Enemy
import Boss
import Bullet
import Sprite
import Score
import GameConstants
import GalagaPics
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import System.IO

-- The four possible screens
-- This is the 'world' type that we modify as we progress in the game.
data GameState
  = MainMenu GalagaPics
    | Game { ship :: Ship, -- ship that player controls
             enemies :: EnemyController, -- enemies to kill
             bosses :: [Boss], -- higher-level enemy
             bullets :: [Bullet], -- ship's & enemies' bullets
             score :: Int, -- points player has earned
             lives :: Int, -- number of lives left
             elapsed :: Float, -- total time since start of level
             advanceElapsed :: Float, -- time since enemies advanced
             shootElapsed :: Float, -- time since enemies fired
             addElapsed :: Float, -- time since new enemies added
             level :: Int, -- level of difficulty of game
             pics :: GalagaPics
           }
    | HighScores GalagaPics
    | GameOver { score :: Int, 
                 scoreIsRecorded :: Bool, 
                 scoreIsRendered :: Bool,
                 pics :: GalagaPics }


window :: Display
window = InWindow "Galaga" screenSize (10,10)

-- The initial state of the program
initState :: GalagaPics -> GameState
initState pics = MainMenu pics

-- The initial state of the game
initGame :: GalagaPics -> GameState
initGame pics = Game {
                  ship = mkShip,
                  enemies = mkEnemies 6,
                  bosses = [],
                  bullets = [],
                  score = 0,
                  lives = 10,
                  elapsed = 0,
                  advanceElapsed = 0,
                  shootElapsed = 0,
                  addElapsed = 0,
                  level = 1,
                  pics = pics
                }

-- Main method for the entire game
main :: IO () 
main = do
  constantPics <- mkGalagaPics
  playIO window black fps (initState constantPics) Main.render eventHandler gameLoop 

{- The render function takes a gamestate and renders it to the screen -}
render :: GameState -> IO Picture 
render (Game { ship = ship, enemies = enemies, bosses = bosses, bullets = bullets, 
               score = score, lives = lives, level = level, pics = pics }) =
  renderGame ship enemies bosses bullets score lives level pics
render (HighScores _) =  Score.render
render (GameOver score _ _ _) = renderGameOver score
render (MainMenu pics) = renderMainMenu $ menuPic pics

-- Renders picture for game that's in play
renderGame :: Ship -> EnemyController -> [Boss] -> [Bullet] -> Int -> Int -> 
              Int -> GalagaPics -> IO Picture
renderGame ship enemies bosses bullets score lives level pics = do
  shipPic <- Ship.render pics ship
  let scorePic = Translate (-370) 250 $ Scale 0.15 0.15 $ 
                 Color white (Text $ "SCORE: " ++ (show score))
      livesPic = Translate 280 250 $ Scale 0.15 0.15 $ 
                 Color white (Text $ "LIVES: " ++ (show lives))
      levelPic = Translate (-100) 250 $ Scale 0.15 0.15 $ 
                 Color white (Text $ "LEVEL: " ++ (show level))
  enemiesPic <- Pictures <$> EnemyController.render enemies pics
  bulletsPic <- Pictures <$> mapM (Bullet.render pics) bullets
  bossesPic <- Pictures <$> mapM (Boss.render pics) bosses
  return $ Pictures [scorePic, livesPic, levelPic, bossesPic, shipPic,
                      enemiesPic, bulletsPic]

-- Renders game over screen
renderGameOver :: Int -> IO Picture
renderGameOver score = do
  let titlePic = Translate (-250) 100 $ Scale 0.4 0.4 $ 
                 Color white (Text $ "Game Over")
      scorePic = Translate (-250) (0) $ Scale 0.4 0.4 $ 
                 Color green (Text $ "Final Score: " ++ (show score))
      notePic = Translate (-250) (-100) $ Scale 0.2 0.2 $ 
                 Color white (Text $ "check console to see if you got a high score")
      instructionsPic = Translate (-250) (-200) $ Scale 0.2 0.2 $ 
                 Color yellow (Text $ "press r to replay, m for menu")
  return $ Pictures [titlePic, scorePic, notePic, instructionsPic]

-- Renders main menu screen
renderMainMenu :: Picture -> IO Picture
renderMainMenu menuPic = do
  let galagaPic = Translate 0 100 $ Scale 0.4 0.4 menuPic
      titlePic = Translate (-250) (-100) $ Scale 0.2 0.2 $ 
                 Color green (Text $ "Main Menu")
      instructionsPic = Translate (-250) (-150) $ Scale 0.2 0.2 $ 
                 Color white (Text $ "press s to start, h for high scores")
  return $ Pictures [galagaPic, titlePic, instructionsPic]

{- The event handlers handles events coming from the user -}
eventHandler :: Event -> GameState -> IO GameState 
{- This pattern matches for Key Events -}
eventHandler (EventKey (Char key) Up _ _) state = case state of
  game@(Game { score = score, pics = pics }) -> do -- in-progress game screen
    case key of 
      'q' -> return $ GameOver score False False pics -- force game to be over
      _  -> return game
  gameover@(GameOver score _ _ pics) -> do -- gameover screen
    case key of
      'r' -> return $ initGame pics -- restart the game
      'm' -> return $ MainMenu pics -- show menu
      _ -> return gameover
  MainMenu pics -> do -- main menu screen
    case key of
      's' -> return $ initGame pics -- start game
      'h' -> return $ HighScores pics -- show scores
      _ -> return $ MainMenu pics
  HighScores pics -> do -- high scores screen
    case key of
      'm' -> return $ MainMenu pics
      _ -> return $ HighScores pics

{- This pattern matches for Special key Events-}
eventHandler (EventKey (SpecialKey key) Up _ _) state = case state of  
  game@(Game { ship = ship, bullets = bullets }) ->
    if (isActive ship)
      then case key of
        KeyLeft -> return $ game { ship = makeStationary ship }
        KeyRight -> return $ game { ship = makeStationary ship }
        KeySpace -> return $ game { bullets = addShipBullet ship bullets }
        _ -> return game
    else return game
  _ -> return state -- any screen other than in-progress game

-- Force an error to make the game exit
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) _ = error "Exited game"

eventHandler (EventKey (SpecialKey key) Down _ _) state = case state of  
  game@(Game { ship = ship, bullets = bullets }) -> 
    if (isActive ship)
      then case key of
        KeyLeft -> return $ game { ship = moveLeft ship }
        KeyRight -> return $ game { ship = moveRight ship }
        _ -> return game
    else return game
  _ -> return state

{- The catch all pattern -}
eventHandler _ state = return state

-- Checks if the ship got damaged to update lives and ship state
checkShipDamage :: [Bullet] -> EnemyController -> [Boss] -> Ship -> Int -> 
                   (Int, Ship)
checkShipDamage bullets controller bosses ship lives
  | shipDamaged = (lives - 1, ship { Ship.state = (Exploding 1) })
  | otherwise = (lives, ship)
  where shipDamaged = (Ship.state ship == Alive) && damagedByFoe
        damagedByFoe = (isDamagedBy detectCollision ship enemies)
                       || (isDamagedBy detectCollision ship enemyBullets)
                       || (isDamagedBy isCapturedByBeam ship bosses)
        enemyBullets = filter (isEnemyBullet) bullets
        enemies = getEnemies controller

-- Advance enemies if possible every 3 seconds
-- Number of enemies advanced each time = current level
advanceEnemies :: Int -> Float -> EnemyController -> (EnemyController, Float)
advanceEnemies level elapsed enemies
  | elapsed > 3 = (advance level enemies, 0)
  | otherwise = (enemies, elapsed)

-- Checks elapsed time to see if enemies should fire bullets
-- Frequency of firing proportional to level
fireEnemyBullets :: Int -> EnemyController -> Float -> [Bullet] -> 
                    ([Bullet], Float)
fireEnemyBullets level enemies elapsed bullets
  | elapsed > interval = (bullets', 0)
  | otherwise = (bullets, elapsed)
  where bullets' = foldr addEnemyBullet bullets advancingEnemies
        advancingEnemies = filter Enemy.isAdvancing (getEnemies enemies)
        interval = max 0.4 (1 - (fromIntegral level) * 0.05)

-- Checks elapsed time to see if new enemies should be added to formation
-- Number of enemies added = current level * 2
-- Frequency of adding enemies is proportional to current level
-- Also adds new enemies if formation is empty
addEnemies :: Int -> EnemyController -> Float -> (EnemyController, Float)
addEnemies level controller elapsed
  | (allKilled controller) || (elapsed > interval) 
      = (expandFormation numEnemiesToAdvance controller, 0)
  | otherwise = (controller, elapsed)
  where interval = max 2 (5 - (fromIntegral level * 0.5))
        numEnemiesToAdvance = level * 2

-- Checks elapsed time to see if should add new boss enemy
-- Maximum number of boss enemies = (current level / 2)
addBoss :: Int -> Float -> [Boss] -> [Boss]
addBoss level elapsed bosses
  | (elapsed > interval) && (length bosses < maxNumBosses) = mkBoss:bosses
  | otherwise = bosses
  where interval = max 2 (5 - (fromIntegral level * 0.5))
        maxNumBosses = level `div` 2

-- Increase the level every 30 seconds / when all enemies killed
updateLevel :: Int -> Float -> EnemyController -> (Int, Float)
updateLevel level elapsed controller
  | (allKilled controller) || (elapsed > levelInterval) = (level + 1, 0)
  | otherwise = (level, elapsed)

{- The game loop for Galaga -}
-- deltaTime: time-based animation. use how much time has passed since the past 
-- frame to move the sprite
gameLoop :: Float -> GameState -> IO GameState 
gameLoop deltaTime game@(Game ship enemies bosses bullets score lives elapsed 
                         advanceElapsed shootElapsed addElapsed level pics) = do
  let elapsed' = elapsed + deltaTime
      advanceElapsed' = advanceElapsed + deltaTime
      shootElapsed' = shootElapsed + deltaTime
      addElapsed' = addElapsed + deltaTime
  -- Remove enemies and bullets that have moved off screen
      enemies' = clearOffScreen $ EnemyController.update deltaTime enemies
      bullets' = eliminateOffScreen $ map (Bullet.update deltaTime) bullets
      bosses' = eliminateOffScreen $ map (Boss.update deltaTime) bosses
  -- Check if ship got damaged or captured
      (newLives, ship') = checkShipDamage bullets' enemies' bosses ship lives
      newShip = Ship.update ship' deltaTime
  -- Remove those enemies and enemy bullets that got destroyed
      shipBullets = filter isShipBullet bullets'
      (enemies'', numEnemiesDestroyed) = EnemyController.markAndCountDestroyed 
                                          enemies' shipBullets ship
      enemies''' = EnemyController.removeDead enemies''
      (bosses'', numBossDestroyed) = Boss.markAndCountDestroyed bosses' shipBullets
                                    ship
      bosses''' = Boss.removeDead bosses''
      newScore = score + (numEnemiesDestroyed * enemyKillPoints) +
                  (numBossDestroyed * bossKillPoints)
      bullets'' = Bullet.removeDestroyed bullets' (getEnemies enemies') 
                  bosses ship
  -- Advance some enemies based on elapsed time
      (enemies'''', newAdvanceElapsed) = advanceEnemies level 
                                          advanceElapsed' enemies'''
  -- Fire bullets from advancing enemies
      (newBullets, newShootElapsed) = fireEnemyBullets level enemies'''' 
                                      shootElapsed' bullets''
  -- Add new enemies based on elapsed time
      (newEnemies, newAddElapsed) = addEnemies level enemies'''' addElapsed'
      newBoss = addBoss level addElapsed' bosses'''
  -- Increase the level at specified intervals / once all enemies are killed
      (newLevel, newElapsed) = updateLevel level elapsed' enemies''
      gameIsOver = newLives == 0
  if gameIsOver
    then return $ GameOver newScore False True pics
    else return $ Game newShip newEnemies newBoss newBullets newScore newLives 
         newElapsed newAdvanceElapsed newShootElapsed newAddElapsed newLevel pics
gameLoop _ (GameOver score False False pics) = 
  return $ GameOver score False True pics
gameLoop _ (GameOver score False True pics) = do
  updateHighScores score
  return $ GameOver score True True pics
gameLoop _ state = return state