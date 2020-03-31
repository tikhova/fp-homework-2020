module Block6.Task4Spec (spec) where

import           Block6.Task1
import           Block6.Task4
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "listlistParser" $ do
    it "returns Just ([ [1, 10], [5, -7, 2] ], \"\") on \"2, 1,+10  , 3,5,-7, 2\"" $
      runParser listlistParser "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([ [1, 10], [5, -7, 2] ], "")

    it "returns Nothing if comma is missing" $
      runParser listlistParser "2, 1,+10   3,5,-7, 2" `shouldBe` Nothing

    it "returns Just ([],\"\") on empty input" $
      runParser listlistParser "" `shouldBe` Just ([],"")
