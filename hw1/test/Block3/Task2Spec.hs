module Block3.Task2Spec (spec) where

import           Block3.Task2
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "ThisOrThat Semigroup" $ do
    it "(<>) (This [True]) (That ()) returns Both [True] ()" $
      (<>) (This [True]) (That ()) `shouldBe` Both [True] ()

    it "(<>) (This [True]) (That ()) equals (<>) (That ()) (This [True])" $
      (<>) (This [True] ) (That ()) `shouldBe` (<>) (That ()) (This [True])

  describe "Name Semigroup" $ do
    it "Name \"Kit\" <> Name \"Kat\" returns Name \"Kit.Kat\"" $
      Name "Kit" <> Name "Kat" `shouldBe` Name "Kit.Kat"

    it "(Name \"Kit\" <> Name \"ty\") <> Name \"Kat\" equals Name \"Kit\" <> (Name \"ty\" <> Name \"Kat\")" $
      (Name "Kit" <> Name "ty") <> Name "Kat" `shouldBe` Name "Kit" <> (Name "ty" <> Name "Kat")

  describe "Name Monoid" $ do
    it "Name \"Kit\" <> mempty returns Name \"Kit\"" $
      Name "Kit" `mappend` mempty `shouldBe` Name "Kit"

    it "mempty <> Name \"Kit\" returns Name \"Kit\"" $
      mempty `mappend` Name "Kit" `shouldBe` Name "Kit"

  describe "Endo Semigroup" $
    it "(Endo (+ 1) <> Endo (* 2)) <> Endo (div 3) equals Name Endo (+ 1) <> (Endo (* 2) <> Endo (div 3))" $ do
      let f = getEndo (((Endo (+ 1) :: Endo Int) <> (Endo (* 2) :: Endo Int)) <> (Endo (div 3) :: Endo Int)) 6
      let s = getEndo ((Endo (+ 1) :: Endo Int) <> ((Endo (* 2) :: Endo Int) <> (Endo (div 3) :: Endo Int))) 6
      f `shouldBe` s

  describe "Endo Monoid" $ do
    it "Endo (+ 1) <> mempty equals Endo (+ 1)" $ do
      let f = getEndo ((Endo (+ 1) :: Endo Int) `mappend` (mempty :: Endo Int)) 0
      let s = getEndo (Endo (+ 1) :: Endo Int) 0
      f `shouldBe` s

    it "mempty <> Endo (+ 1) <> equals Endo (+ 1)" $ do
      let f = getEndo ((mempty :: Endo Int) `mappend` (Endo (+ 1) :: Endo Int)) 0
      let s = getEndo (Endo (+ 1) :: Endo Int) 0
      f `shouldBe` s
