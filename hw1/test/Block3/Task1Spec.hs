module Block3.Task1Spec (spec) where

import           Block3.Task1
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "maybeConcat" $
    it "maybeConcat [Just [True,True,False], Nothing, Just [True,False]] returns [True,True,False,True,False]" $
      maybeConcat [Just [True,True,False], Nothing, Just [True,False]] `shouldBe` [True,True,False,True,False]
