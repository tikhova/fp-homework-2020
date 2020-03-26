module Block4.Task1Spec (spec) where

import           Block4.Task1
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "stringSum" $ do
    it "returns 10 when given 1, 2, 3, 4" $
      stringSum "  1 2  3 4" `shouldBe` Just 10

  describe "stringSum" $ do
    it "returns 0 when given empty string" $
      stringSum "" `shouldBe` Just 0
