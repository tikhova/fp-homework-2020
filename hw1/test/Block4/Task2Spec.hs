module Block4.Task2Spec (spec) where

import           Block4.Task2
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Functor Tree" $ do
    it "fmap id equals id" $ do
      let t = Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 0) :: Tree Int
      fmap id t `shouldBe` id t

    it "fmap (f . g) equals fmap f . fmap g" $ do
      let t = Branch (Branch (Leaf 5) (Leaf 3)) (Leaf 0) :: Tree Int
      fmap (show . (+1)) t `shouldBe` (fmap show . fmap (+1)) t

  describe "Applicative Tree" $ do
    it "identity" $ do
      let t = Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 0) :: Tree Int
      pure id <*> t `shouldBe` t

    it "composition" $ do
      let u = Branch (Branch (Leaf (+ 3)) (Leaf (* 1))) (Leaf (* 2)) :: Tree (Int -> Int)
      let v = Branch (Branch (Leaf (+ 5)) (Leaf (* 3))) (Leaf (+ 1)) :: Tree (Int -> Int)
      let w = Branch (Leaf 0) (Branch (Leaf 6) (Leaf 7)) :: Tree Int
      pure (.) <*> u <*> v <*> w `shouldBe` u <*> (v <*> w)

    it "homomorphism" $ do
      let f = (+ 2) :: Int -> Int
      let x = 5 :: Int
      (pure f :: Tree (Int -> Int)) <*> (pure x :: Tree Int) `shouldBe` pure (f x)

    it "interchange" $ do
      let u = pure (+ 2) :: Tree (Int -> Int)
      let y = 5 :: Int
      u <*> pure y `shouldBe` pure ($ y) <*> u

  describe "Foldable Tree" $
    it "returns [3, 5, 0] given (:) [] Branch (Branch (Leaf 3) (Leaf 5)) (Leaf 0)" $ do
      let t = Branch (Branch (Leaf 3) (Leaf 5)) (Leaf 0) :: Tree Int
      foldr (:) [] t `shouldBe` [3, 5, 0]

  describe "Traversable Tree" $ do
    it "returns Just given tree with elements taken by mod of 3" $ do
      let t = Branch (Branch (Leaf 3) (Leaf 5)) (Leaf 0) :: Tree Int
      let t' = Branch (Branch (Leaf 0) (Leaf 2)) (Leaf 0) :: Tree Int
      let f x = Just $ x `mod` 3 :: Maybe Int
      traverse f t `shouldBe` Just t'

    it "returns Nothing given (== 0) and Branch (Branch (Leaf 3) (Leaf 5)) (Leaf 0)" $ do
      let t = Branch (Branch (Leaf 3) (Leaf 5)) (Leaf 0) :: Tree Int
      let f x = if x == 0 then Nothing else Just x
      traverse f t `shouldBe` Nothing

