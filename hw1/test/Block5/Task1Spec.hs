module Block5.Task1Spec (spec) where

import           Block5.Task1
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "eval add" $
    it "2 + 2 = 4" $ do
      let two = Const 2
      eval (Add two two) `shouldBe` Right 4

  describe "eval sub" $
    it "2 - 2 = 0" $ do
      let two = Const 2
      eval (Sub two two) `shouldBe` Right 0

  describe "eval mul" $
    it "2 * 2 = 4" $ do
      let two = Const 2
      eval (Mul two two) `shouldBe` Right 4

  describe "eval div" $ do
    it "4 / 2 = 4" $ do
      let two = Const 2
      let four = Const 4
      eval (Div four two) `shouldBe` Right 2

    it "4 / 0 throws error" $ do
      let zero = Const 0
      let four = Const 4
      eval (Div four zero) `shouldBe` Left DivisionByZero

  describe "eval pow" $ do
    it "4 ^ 2 = 16" $ do
      let two = Const 2
      let four = Const 4
      eval (Pow four two) `shouldBe` Right 16

    it "4 ^ 0 = 1" $ do
      let zero = Const 0
      let four = Const 4
      eval (Pow four zero) `shouldBe` Right 1

    it "4 / 0 throws error" $ do
      let mOne = Const (negate 1)
      let four = Const 4
      eval (Pow four mOne) `shouldBe` Left NegativePower

  describe "complex example" $ do
    it "(3 / 2) + (4 * 3) = 13" $ do
      let two = Const 2
      let three = Const 3
      let four = Const 4
      eval (Add (Div three two) (Mul four three)) `shouldBe` Right 13

    it "(3 / 4) + (4 ^ (-1)) throws error" $ do
      let mOne = Const (negate 1)
      let three = Const 3
      let four = Const 4
      eval (Add (Div three four) (Pow four mOne)) `shouldBe` Left NegativePower
