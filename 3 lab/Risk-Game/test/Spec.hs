module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Random
import Control.Monad
import Data.List
import System.IO.Unsafe

import Risk

main :: IO ()
main = hspec $ describe "Testing excercises" $ do
    describe "Excercise 2" $
      it "Army decrease" $
        property $ do
          let a = 10
              d = 10
          checkDecrease (a, d) (returnVal $ battle (Battlefield a d))

    describe "Excercise 3" $
      it "Army destroyed" $ 
        property $ checkDestroyed $ returnVal $ invade (Battlefield 10 10)

    describe "Excercise 4" $
      it "1 probability with no defenders" $
        returnVal (successProb (Battlefield 10 0)) `shouldBe` (1.0 :: Double)

    describe "Excercise 4" $
      it "0 probability with no attackers" $
         returnVal (successProb (Battlefield 0 10)) `shouldBe` (0.0 :: Double)

    describe "Excercise 4" $
      it "from 0 to 1" $
        property $ prop_range (returnVal (successProb (Battlefield 10 10)))

-- prop_ex :: Battlefield -> Rand StdGen Double
returnVal = unsafePerformIO . evalRandIO

prop_range :: Double -> Bool 
prop_range val = val > 0.0 && val < 1.0

checkDecrease :: (Army, Army) -> Battlefield -> Bool
checkDecrease (a, d) bf = attackers bf < a || defenders bf < d

checkDestroyed :: Battlefield -> Bool
checkDestroyed bf = attackers bf == 1 || defenders bf == 0