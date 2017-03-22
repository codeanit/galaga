{-
  File      :  Score.hs
  Copyright : (c) E-Lynn Yap, 3/3/17
  Methods for computing, updating and rendering the high scores in the game.
-}

module Score
(
  render,
  updateHighScores
) where

import System.IO
import Graphics.Gloss
import GameConstants
import Data.List(sortOn)

render :: IO Picture
render = do
  scoreStrings <- getScores
  let titlePic = Translate (-200) 200 $ Scale 0.25 0.25 $ 
                 Color green (Text $ "High Scores")
      instructionPic = Translate (-200) 160 $ Scale 0.15 0.15 $ 
                        Color yellow (Text $ "press m to return to menu")
      scorePics = fst $ foldr f ([], 120) scoreStrings
      f score (scores, y) = ((Translate (-200) y $ Scale 0.15 0.15 $ 
                            Color white (Text $ score)):scores, y-40)
  return $ Pictures (instructionPic:titlePic:scorePics)

updateHighScores :: Int -> IO ()
updateHighScores score = do
  scoreStrings <- getScores
  let scoreVals = map (last . words) scoreStrings
  if (isHighScore score scoreVals)
    then do
      putStrLn "Congratulations, you earned a high score!"
      putStrLn "Type in your name and hit enter (max 15 chars)."
      name <- (take 15) <$> getLine
      addHighScore name score scoreStrings
      putStrLn "High score recorded!"
    else putStrLn "You didn't get a high score, sorry."

-- Checks a score against a list of current high scores to see if it's
-- a new high score
isHighScore :: Int -> [String] -> Bool
isHighScore score scoreVals
  | (length scoreVals) < highScoresNum = True
  | otherwise = score > lowestHighScore
  where lowestHighScore = minimum $ map (\x -> read x :: Int) scoreVals

-- Returns the saved scores as a list of line-separated strings
getScores :: IO [String]
getScores = lines <$> readFile highScoresFile

-- Adds a new high score to the file
addHighScore :: String -> Int -> [String] -> IO ()
addHighScore name score existingScores = do
  let scores' = (name ++ " " ++ (show score)):existingScores
      scores'' = sortOn f scores'
      f scoreStr = (read (last $ words scoreStr)) :: Int
      newScores = if (length scores'' > highScoresNum)
                  then drop 1 scores''
                  else scores''
  writeFile highScoresFile (unlines newScores)