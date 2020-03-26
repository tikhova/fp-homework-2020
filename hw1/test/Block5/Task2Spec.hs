module Block5.Task2Spec (spec) where

import           Block5.Task2
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "moving" $ do
    it "moving 4 [] == []" $
      moving 4 [] `shouldBe` []

    it "moving 1 [1, 5, 3, 8, 7, 9, 6] == [1, 5, 3, 8, 7, 9, 6]" $
      moving 1 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1, 5, 3, 8, 7, 9, 6]

    it "moving 3 [1, 5, 3, 8, 7, 9, 6] == [1, 5, 3, 8, 7, 9, 6]" $
      moving 1 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.3, 6.0, 8.0, 7.3]

    it "moving 4 [1, 5, 3, 8, 7, 9, 6] == [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]" $
      moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
