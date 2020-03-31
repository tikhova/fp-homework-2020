module Block6.Task1Spec (spec) where

import           Block6.Task1
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Functor" $ do
    it "fmap id ≡ id" $ do
      let parser = pure True :: Parser Char Bool
      runParser (fmap id parser) "" `shouldBe` runParser (id parser) ""

    it "fmap (f . g) ≡ fmap f . fmap g" $ do
      let parser = pure True :: Parser Char Bool
      runParser (fmap (id . not) parser) "" `shouldBe` runParser ((fmap id . fmap not) parser) ""

  describe "Applicative" $ do
    it "identity" $ do
      let parser = pure True :: Parser Char Bool
      runParser (fmap id parser) "" `shouldBe` runParser (id parser) ""

    it "composition" $ do
      let u = pure (const 3) :: Parser Char (Bool -> Int)
      let v = pure (const True) :: Parser Char (Int -> Bool)
      let w = pure 1 :: Parser Char Int
      let left = pure (.) <*> u <*> v <*> w
      let right = u <*> (v <*> w)
      runParser left "" `shouldBe` runParser right ""

    it "homomorphism" $ do
      let v = const True :: Int -> Bool
      let w = 1 :: Int
      let left = pure v <*> pure w
      let right = pure (v w)
      runParser left "" `shouldBe` runParser right ""

    it "interchange" $ do
      let u = pure (const True) :: Parser Char (Int -> Bool)
      let y = 1 :: Int
      let left = u <*> pure y
      let right = pure ($ y) <*> u
      runParser left "" `shouldBe` runParser right ""

  describe "Applicative" $ do
    it "left identity" $ do
      let parser = return True :: Parser Char Bool
      let f x = return (not x) :: Parser Char Bool
      let left = parser >>= f
      let right = f True
      runParser left "" `shouldBe` runParser right ""

    it "right identity" $ do
      let parser = return True :: Parser Char Bool
      let left = parser >>= return
      let right = parser
      runParser left "" `shouldBe` runParser right ""

    it "associativity" $ do
      let m = return True :: Parser Char Bool
      let f x = return (not x) :: Parser Char Bool
      let g x = return (if x then 1 else 0) :: Parser Char Int
      let left = (m >>= f) >>= g
      let right = m >>= (\x -> f x >>= g)
      runParser left "" `shouldBe` runParser right ""
