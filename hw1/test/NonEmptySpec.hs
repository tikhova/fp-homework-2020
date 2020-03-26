module NonEmptySpec (spec) where

import           NonEmpty
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec = 
  describe "NonEmpty Semigroup" $
    it "(True :| [False]) <> (False :| [True]) returns True :| [False, False, True]" $
      (True :| [False]) <> (False :| [True]) `shouldBe` (True :| [False, False, True])