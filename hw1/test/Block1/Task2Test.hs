module Block1.Task2Test (spec) where
import Hedgehog

import Block1.Task2
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 0 100000
  in  Gen.list listLength Gen.enumBounded

prop_add :: Property
prop_add = property $ do
    xs <- forAll genIntList
    ys <- forAll genIntList
    xs + ys === natToInt $ add (intToNat xs) (intToNat ys)

tests :: IO Bool
tests =
  checkParallel $$(discover)