module Makegen.Internal.Stable
  ( Stable(stableNameOf, unStable)
  , StableName
  ) where
import Data.Functor.Classes (Show1(showsPrec1), showsUnary, showsBinary1)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName)

data Stable a =
  Stable
  { stableNameOf :: !(StableName a)
  , unStable :: !a
  }

instance Show a => Show (Stable a) where
  showsPrec = showsPrec1

instance Show1 Stable where
  showsPrec1 d (Stable _ x) = showsUnary "pure" d x

instance Functor Stable where
  fmap f = (pure f <*>)

instance Applicative Stable where
  pure x = Stable (unsafePerformIO (makeStableName x)) x
  mf <*> mx = mf >>= \ f -> mx >>= \ x -> pure (f x)

instance Monad Stable where
  return = pure
  mx >>= f = f (unStable mx)

instance Monoid a => Monoid (Stable a) where
  mempty = pure mempty
  x `mappend` y = mappend <$> x <*> y
