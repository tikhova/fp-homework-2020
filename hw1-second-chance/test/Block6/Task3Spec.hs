module Block6.Task3Spec (spec) where

import           Block6.Task1
import           Block6.Task3
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "bsParser" $ do
    it "returns Just ((), \"\") on \"()()\"" $
      runParser bsParser "()()" `shouldBe`Just ((), "")

    it "returns Just ((), \"\") on \"(())\"" $
      runParser bsParser "(())" `shouldBe`Just ((), "")

    it "returns Just ((), \"\") on \"(())(())\"" $
      runParser bsParser "(())(())" `shouldBe`Just ((), "")

    it "returns Nothing  on \"(( ()() ))\"" $
      runParser bsParser "(( ()() ))" `shouldBe` Nothing

  describe "intParser" $ do
    it "returns Just (1, \" 1\") on \"1 1\"" $
      runParser intParser "1 1" `shouldBe` Just (1, " 1")

    it "returns Just (42, \" 33\") on \"42 33\"" $
      runParser intParser "42 33" `shouldBe` Just (42, " 33")

    it "returns Nothing on \" 33\"" $
      runParser intParser " 33" `shouldBe` Nothing

    it "returns Nothing on \"\"" $
      runParser intParser "" `shouldBe` Nothing

    it "returns Nothing on \"+\"" $
      runParser intParser "+" `shouldBe` Nothing

    it "returns Nothing on \"-\"" $
      runParser intParser "-" `shouldBe` Nothing


    it "returns Just (100, \"\") on \"+100\"" $
      runParser intParser "+100" `shouldBe` Just (100, "")

    it "returns Just (-100, \"\") on \"-100\"" $
      runParser intParser "-100" `shouldBe` Just (-100, "")
