module Block1.Task2Spec (spec) where

import           Block1.Task2
import           Control.Exception (evaluate)
import           NonEmpty          ()
import           Test.Hspec        (Spec, anyException, describe, it, shouldBe,
                                    shouldThrow)

spec :: Spec
spec = do
  describe "add" $ do
    it "returns zero when given zero and zero" $
      add Z Z `shouldBe` Z

    it "returns one when given zero and one" $
      add Z (S Z) `shouldBe` S Z

    it "returns one when given one and zero" $
      add (S Z) Z `shouldBe` S Z

    it "returns four when given two and two" $
      add (S $ S Z) (S $ S Z) `shouldBe` S (S $ S $ S Z)

  describe "sub" $ do
    it "returns zero when given zero and zero" $
      sub Z Z `shouldBe` Z

    it "returns zero when given zero and one" $
      sub Z (S Z) `shouldBe` Z

    it "returns one when given one and zero" $
      sub (S Z) Z `shouldBe` S Z

  describe "mul" $ do
    it "returns zero when given zero and zero" $
      mul Z Z `shouldBe` Z

    it "returns zero when given one and zero" $
      mul (S Z) Z `shouldBe` Z

    it "returns zero when given zero and one" $
      mul Z (S Z) `shouldBe` Z

    it "returns one when given one and one" $
      mul (S Z) (S Z) `shouldBe` S Z

    it "returns four when given two and two" $
      mul (S $ S Z) (S $ S Z) `shouldBe` S (S $ S $ S Z)

  describe "divide" $ do
    it "returns zero when given zero and one" $
      divide Z (S Z) `shouldBe` Z

    it "returns one when given three and two" $
      divide (S $ S $ S Z) (S $ S Z) `shouldBe` S Z

    it "throws an exception when given one and zero" $
      evaluate (divide (S Z) Z) `shouldThrow` anyException

    it "returns two when given four and two" $
      divide (S $ S $ S $ S Z) (S $ S Z) `shouldBe` S (S Z)

  describe "isEven" $ do
    it "returns true when given zero" $
      isEven Z `shouldBe` True

    it "returns false when given one" $
      isEven (S Z) `shouldBe` False

    it "returns true when given two" $
      isEven (S $ S Z) `shouldBe` True

  describe "natMod" $ do
    it "returns zero when given zero and one" $
      natMod Z (S Z) `shouldBe` Z

    it "returns one when given three and two" $
      natMod (S $ S $ S Z) (S $ S Z) `shouldBe` S Z

    it "throws an exception when given one and zero" $
      evaluate (natMod (S Z) Z) `shouldThrow` anyException

    it "returns zero when given four and two" $
      natMod (S $ S $ S $ S Z) (S $ S Z) `shouldBe` Z

  describe "natToInt" $ do
    it "returns 0 when given Z" $
      natToInt Z `shouldBe` 0

    it "returns 1 when given S Z" $
      natToInt (S Z) `shouldBe` 1

    it "returns 2 when given S S Z" $
      natToInt (S $ S Z) `shouldBe` 2

  describe "intToNat" $ do
    it "returns Z when given 0" $
      intToNat 0 `shouldBe` Z

    it "returns S Z when given 1" $
      intToNat 1 `shouldBe` S Z

    it "returns S S Z when given 2" $
      intToNat 2 `shouldBe` S (S Z)

    it "throws an exception when given -1" $
      evaluate (intToNat $ -1) `shouldThrow` anyException

  describe "eq" $ do
    it "returns True when given Z and Z" $
      Z == Z `shouldBe` True

    it "returns True when given S Z and S Z" $
      S Z == S Z `shouldBe` True

    it "returns False when given S Z and Z" $
      S Z == Z `shouldBe` False

    it "returns False when given Z and S Z" $
      Z == S Z `shouldBe` False

  describe "ord" $ do
    it "returns False when given Z < Z" $
      Z < Z `shouldBe` False

    it "returns False when given S Z > S Z" $
      S Z > S Z `shouldBe` False

    it "returns True when given Z < S Z" $
      Z < S Z `shouldBe` True
