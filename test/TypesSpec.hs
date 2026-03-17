module TypesSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Types

newtype ArbReward = ArbReward Reward
  deriving Show

instance Arbitrary ArbReward where
  arbitrary =
    fmap ArbReward $
      Reward
        <$> nonNeg
        <*> nonNeg
        <*> nonNeg
        <*> nonNeg
        <*> nonNeg
        <*> nonNeg
    where
      nonNeg = getNonNegative <$> arbitrary

spec :: Spec
spec = do
  describe "Reward algebra" $ do
    it "zeroReward is left identity" $
      property $ \(ArbReward r) ->
        addReward zeroReward r == r

    it "zeroReward is right identity" $
      property $ \(ArbReward r) ->
        addReward r zeroReward == r

    it "addReward is associative" $
      property $ \(ArbReward a) (ArbReward b) (ArbReward c) ->
        addReward a (addReward b c) == addReward (addReward a b) c

  describe "defaultValuation" $ do
    it "values 5000 EXP as 100 gold" $
      scoreReward defaultValuation zeroReward { rewardExp = 5000 } `shouldBe` 100

    it "values one minor star as 90 gold" $
      scoreReward defaultValuation zeroReward { rewardMinorStars = 1 } `shouldBe` 90

    it "values one major star as 120 gold" $
      scoreReward defaultValuation zeroReward { rewardMajorStars = 1 } `shouldBe` 120

    it "values 500 AE as 100 gold" $
      scoreReward defaultValuation zeroReward { rewardArclightEnergy = 500 } `shouldBe` 100

    it "values one Rare Core as 250 gold" $
      scoreReward defaultValuation zeroReward { rewardRareCores = 1 } `shouldBe` 250

    it "values leftover tickets as zero by default" $
      scoreLeftoverTickets defaultValuation 500 `shouldBe` 0

  describe "scoreReward" $ do
    it "scores gold directly" $
      scoreReward defaultValuation zeroReward { rewardGold = 250 } `shouldBe` 250

    it "combines reward components additively" $ do
      let sampleReward =
            Reward
              { rewardGold = 50
              , rewardExp = 5000
              , rewardMinorStars = 1
              , rewardMajorStars = 1
              , rewardArclightEnergy = 500
              , rewardRareCores = 1
              }

      scoreReward defaultValuation sampleReward `shouldBe` 710