module Block2.Task2Spec (spec) where

import           Block2.Task2
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "splitOn and joinWith" $ do
    it "joinWith '/' (splitOn '/' \"path/to/file\") returns \"path/to/file\"" $
      joinWith '/' (splitOn '/' "path/to/file") `shouldBe` "path/to/file"

    it "joinWith '+' (splitOn '-' \"\") returns \"\"" $
      joinWith '+' (splitOn '-' "") `shouldBe` ""
