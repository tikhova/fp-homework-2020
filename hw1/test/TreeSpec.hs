module TreeSpec (spec) where

import           Tree
import           NonEmpty
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "isEmpty" $ do
    it "returns True when given Leaf" $
      isEmpty Leaf `shouldBe` True

    it "returns False when given Node" $
      isEmpty (Node (True :| []) Leaf Leaf) `shouldBe` False

  describe "size" $ do
    it "returns 0 when given Leaf" $
      size Leaf `shouldBe` 0

    it "returns 1 when given Node with a singleton" $
      size (Node (True :| []) Leaf Leaf) `shouldBe` 1


    it "returns 4 when given Node with a list of 4 True" $
      size (Node (True :| [True, True, True]) Leaf Leaf) `shouldBe` 4

  describe "insert" $ do
    it "returns tree of size 1 when given True and Leaf" $
      size (insert True Leaf) `shouldBe` 1

    it "returns tree of size 2 when performed twice" $
      size (insert True $ insert False Leaf) `shouldBe` 2

  describe "fromList" $ do
    it "returns tree of size 2 given list [True, False]" $
      size (fromList [True, False]) `shouldBe` 2

    it "returns tree of size 3 given list [True, True, True]" $
      size (fromList [True, True, True]) `shouldBe` 3

  describe "delete" $ do
    it "returns tree of size 0 (Leaf) given True and Leaf" $
      size (delete True Leaf) `shouldBe` 0

    it "returns tree of size 0 (Leaf) given True and Node with True" $
      size (delete True (insert True Leaf)) `shouldBe` 0

