module Block1.Task1Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Block1.Task1

spec :: Spec
spec = do
  describe "nextDay" $ do
    it "returns Monday when given Sunday" $
      nextDay Sunday `shouldBe` Monday

    it "returns Tuesday when given Monday" $
      nextDay Monday `shouldBe` Tuesday

    it "returns Tuesday when given Monday" $
      nextDay Monday `shouldBe` Tuesday

    it "returns Wednesday when given Tuesday" $
      nextDay Tuesday `shouldBe` Wednesday

    it "returns Thursday when given Wednesday" $
      nextDay Wednesday `shouldBe` Thursday

    it "returns Friday when given Thursday" $
      nextDay Thursday `shouldBe` Friday

    it "returns Saturday when given Friday" $
      nextDay Friday `shouldBe` Saturday
    
    it "returns Sunday when given Saturday" $
      nextDay Saturday `shouldBe` Sunday

  describe "afterDays" $ do
    it "returns Monday given Monday and zero interval" $
      afterDays 0 Monday `shouldBe` Monday

    it "returns Tuesday given Monday and unit interval" $
      afterDays 1 Monday `shouldBe` Tuesday

    it "returns Monday given Monday and week interval" $
      afterDays 7 Monday `shouldBe` Monday

    it "returns Friday given Monday and interval 4" $
      afterDays 4 Monday `shouldBe` Friday

  describe "isWeekend" $ do
    it "returns false given Monday" $
      isWeekend Monday `shouldBe` False

    it "returns true given Saturday" $
      isWeekend Saturday `shouldBe` True

    it "returns true given Sunday" $
      isWeekend Sunday `shouldBe` True

  describe "daysToParty" $ do
    it "returns 0 given Friday" $
      daysToParty Friday `shouldBe` 0

    it "returns 2 given Wednesday" $
      daysToParty Wednesday `shouldBe` 2

    it "returns 5 given Sunday" $
      daysToParty Sunday `shouldBe` 5