module Day1Spec where

import Test.Hspec
import qualified Data.Set as S

parseDelta :: String -> Integer
parseDelta ('+':d) = read d
parseDelta ('-':d) = negate (read d)
parseDelta _ = error "Bad input: must begin with + or -"

freq :: [Integer] -> Integer
freq = sum

freqTest :: [String] -> Integer
freqTest = freq . (parseDelta <$>)

repeats :: [Integer] -> Integer
repeats xs = let init = 0
              in repeatsHelper (cycle xs) (S.singleton init) init
             where 
               repeatsHelper :: [Integer] -> S.Set Integer -> Integer -> Integer
               repeatsHelper (d:ds) seen f = let f' = f + d
                                              in if S.member f' seen then f'
                                                 else repeatsHelper ds (S.insert f' seen) f'

repeatsTest :: [String] -> Integer
repeatsTest = repeats . (parseDelta <$>)

spec :: Spec
spec = do
  it "can add numbers" $ do
    freqTest ["+1", "-2", "+3", "+1"] `shouldBe` 3
    freqTest ["+1", "+1", "+1"] `shouldBe` 3
    freqTest ["+1", "+1", "-2"] `shouldBe` 0
    freqTest ["-1", "-2", "-3"] `shouldBe` -6

  it "can track repeated frequencies" $ do
    repeatsTest ["+1", "-2", "+3", "+1"] `shouldBe` 2
    repeatsTest ["+1", "-1"] `shouldBe` 0
    repeatsTest ["+3", "+3", "+4", "-2", "-4"] `shouldBe` 10
    repeatsTest ["-6", "+3", "+8", "+5", "-6"] `shouldBe` 5
    repeatsTest ["+7", "+7", "-2", "-7", "-4"] `shouldBe` 14

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ "Part 1: " ++ (show (freqTest input))
  print $ "Part 2: " ++ (show (repeatsTest input))

