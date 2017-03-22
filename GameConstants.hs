{- 
   File      :  GameConstants.hs 
   Represents the constant values that will not change throughout the 
   execution of the game. 
-}
module GameConstants 
(
    fps,
    screenSize, 
    screenWidth,
    screenHeight,
    enemyKillPoints,
    levelInterval,
    highScoresFile,
    highScoresNum,
    bossKillPoints,
    Size 
)where 

import Graphics.Gloss

type Size = (Int,Int)

-- The number of frames per second to render. Typically this is 60 fps 
fps :: Int 
fps = 60

-- The name of the file that high scores are saved in
highScoresFile :: String
highScoresFile = "scores.txt"

-- Number of high scores recorded
highScoresNum :: Int
highScoresNum = 10

-- Dimensions of the screen
screenSize :: Size 
screenSize = (800,600)

screenWidth :: Int 
screenWidth = fst screenSize

screenHeight :: Int 
screenHeight = snd screenSize

-- The number of points the player gets for destroying an enemy
enemyKillPoints :: Int
enemyKillPoints = 10

-- Points for destroying a boss
bossKillPoints :: Int
bossKillPoints = 100

-- The interval between new levels (in seconds)
levelInterval :: Float
levelInterval = 15