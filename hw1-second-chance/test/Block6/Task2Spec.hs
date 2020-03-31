module Block6.Task2Spec (spec) where

import           Block6.Task1
import           Block6.Task2
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "ok" $
    it "ok doesn't fall or take input" $
      runParser ok "42" `shouldBe` Just ((), "42")

  describe "eof" $ do
    it "doesn't fall on empty input" $
      runParser eof "" `shouldBe` Just ((), "")

    it "returns Nothing on non-empty input" $
      runParser eof "5" `shouldBe` Nothing

  describe "satisfy" $ do
    let parser = satisfy (\x -> x `mod` 10 == 0) :: Parser Integer Integer

    it "satisfy (\\x -> x `mod` 10 == 0) returns Just (100, [11]) on input [100, 11]" $
      runParser parser [100, 11] `shouldBe` Just (100, [11])

    it "satisfy (\\x -> x `mod` 10 == 0) returns Nothing on input [11, 100]" $
      runParser parser [11, 100] `shouldBe` Nothing

  describe "element" $ do
    let parser = element 100 :: Parser Integer Integer

    it "element 100 returns Just (100, [11]) on input [100, 11]" $
      runParser parser [100, 11] `shouldBe` Just (100, [11])

    it "element 100 returns Nothing on input [11, 100]" $
      runParser parser [11, 100] `shouldBe` Nothing

  describe "element" $ do
    let parser = element 100 :: Parser Integer Integer

    it "element 100 returns Just (100, [11]) on input [100, 11]" $
      runParser parser [100, 11] `shouldBe` Just (100, [11])

    it "element 100 returns Nothing on input [11, 100]" $
      runParser parser [11, 100] `shouldBe` Nothing

  describe "stream" $ do
    let parser = stream "Domodedovo" :: Parser Char String

    it "stream \"Domodedovo\" returns Just (\"Domodedovo\", \" station\") on input \"Domodedovo station\"" $
      runParser parser "Domodedovo station" `shouldBe` Just ("Domodedovo", " station")

    it "stream \"Domodedovo\" returns Nothing on input \"Domosedovo station\"" $
      runParser parser "Domosedovo station" `shouldBe` Nothing
