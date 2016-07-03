{-# LANGUAGE BangPatterns #-}
module Makegen.Internal.Joined where
import Data.Monoid
import Data.Functor.Classes (Show1(showsPrec1), showsUnary, showsBinary1)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Makegen.Internal.Stable

data BinaryTree f a
  = BinaryTreeNode (f (BinaryTree f a)) (f (BinaryTree f a))
  | BinaryTreeLeaf a

instance (Show1 f, Show a) => Show (BinaryTree f a) where
  showsPrec = showsPrec1

instance Show1 f => Show1 (BinaryTree f) where
  showsPrec1 d (BinaryTreeLeaf x) = showsUnary "BinaryTreeLeaf" d x
  showsPrec1 d (BinaryTreeNode l r) = showsBinary1 "BinaryTreeNode" d l r

instance Functor f => Functor (BinaryTree f) where
  fmap f = (pure f <*>)

instance Functor f => Applicative (BinaryTree f) where
  pure = BinaryTreeLeaf
  mf <*> mx = mf >>= \ f -> mx >>= \ x -> pure (f x)

instance Functor f => Monad (BinaryTree f) where
  return = pure
  BinaryTreeLeaf x   >>= f = f x
  BinaryTreeNode l r >>= f = BinaryTreeNode ((>>= f) <$> l) ((>>= f) <$> r)

rawJoinAll :: (b -> a -> b)
           -> Stable (BinaryTree Stable a)
           -> (b, HashSet (StableName (BinaryTree Stable a)))
           -> (b, HashSet (StableName (BinaryTree Stable a)))
rawJoinAll f m (!z, seen) =
  case unStable m of
    BinaryTreeLeaf x ->
      (f z x, seen)
    BinaryTreeNode l r ->
      let name = stableNameOf m in
      if HashSet.member name seen
        then
          (z, seen)
        else
          rawJoinAll f r .
          rawJoinAll f l $
          (z, HashSet.insert name seen)

newtype Joined a = Joined (Stable (BinaryTree Stable a))

instance Functor Joined where
  f `fmap` Joined x = Joined ((f <$>) <$> x)

instance Applicative Joined where
  pure = Joined . pure . pure
  mf <*> mx = mf >>= \ f -> mx >>= \ x -> pure (f x)

instance Monad Joined where
  return = pure
  Joined mmx >>= f =
    Joined . pure $ do
      x <- unStable mmx
      case f x of
        Joined my -> unStable my

instance Monoid a => Monoid (Joined a) where
  mempty = pure mempty
  Joined x `mappend` Joined y = Joined (pure (BinaryTreeNode x y))

joinAllWith :: (b -> a -> b) -> b -> Joined a -> b
joinAllWith f z (Joined m) = fst (rawJoinAll f m (z, mempty))

-- | Note that 'a' is required to be not simply a 'Monoid', but also a
--   semilattice.
joinAll :: Monoid a => Joined a -> a
joinAll = joinAllWith (<>) mempty
