{-# LANGUAGE ScopedTypeVariables #-}
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck
import Makegen.Internal.Joined
import Makegen

main = hspec $ do

  describe "Joined" $ do

    it "passes a simple test" $ do
      property $ \ (sets :: [Set Int]) ->
        joinAll (mconcat (pure <$> sets)) == mconcat sets

    it "works on a circular data structure" $ do
      property $ \ (set1 :: Set Int) (set2 :: Set Int) ->
        let m = pure set1 <> m <> pure set2 in
        joinAll m == set1 <> set2
