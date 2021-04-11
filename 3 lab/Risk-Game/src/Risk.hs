{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

------------------------------------------------------------
-- Exercise 2 (Write a function with simulates a single battle)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield a d) =
  do
    let armyAttackers = if a > 3 then 3 else min (a - 1) 3
    let armyDefenders = if d > 2 then 2 else d
    aRolls <- sortedRolls armyAttackers
    dRolls <- sortedRolls armyDefenders
    return $ foldl doStep bf (zip aRolls dRolls)

sortedRolls :: Int -> Rand StdGen [DieValue]
sortedRolls n = sortBy (flip compare) <$> replicateM n getRandom

doStep :: Battlefield -> (DieValue, DieValue) -> Battlefield
doStep (Battlefield a d) (attack, defense)
  | attack > defense = Battlefield a (d - 1)
  | otherwise = Battlefield (a - 1) d

------------------------------------------------------------
-- Exercise 3 (Implement a function which simulates an entire invasion attempt)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a d)
  | a <= 1 || d <= 0 = return bf
  | otherwise = battle bf >>= invade

------------------------------------------------------------
-- Exercise 4 (Implement a function which compute the estimated probability that
-- the attacking army will completely destroy the defending army)

successProb :: Battlefield -> Rand StdGen Double
successProb bf@(Battlefield a d) = do 
  battles <- replicateM 1000 (invade bf)
  return $ fromIntegral (length $ filter ((== 0) . defenders) battles) / 1000.0

------------------------------------------------------------

showGame :: IO ()
showGame = do
  values <- evalRandIO $ invade (Battlefield 10 10)
  prob <- evalRandIO $ successProb (Battlefield 10 10)
  print values
  print prob