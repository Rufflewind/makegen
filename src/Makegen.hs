{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
module Makegen
  ( module Makegen
  , mempty
  , (<>)
  ) where
import Control.Arrow
import Control.Monad
import Data.Dynamic
import Data.Maybe
import Data.Functor.Classes (Show1(showsPrec1), showsUnary, showsBinary1)
import Data.Monoid
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import System.Mem.StableName (StableName, makeStableName)
import System.IO.Unsafe (unsafePerformIO)
import Algebra.Lattice
  ( JoinSemiLattice((\/))
  , BoundedJoinSemiLattice(bottom)
  , joins )
import qualified Crypto.Hash as CryptoHash
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Equivalent to @Control.Lens.At.at@ for 'Map'.
at_Map :: (Functor f, Ord k) =>
          k -> (Maybe a -> f (Maybe a)) -> Map k a -> f (Map k a)
at_Map k f m = set <$> f (Map.lookup k m)
  where
    set Nothing  = Map.delete k m
    set (Just x) = Map.insert k x m

data DigestCached a = DigestCached (Digest a) !a

toDigestCached :: Digestible a => a -> DigestCached a
toDigestCached m = DigestCached (digest m) m

unDigestCached :: DigestCached a -> a
unDigestCached (DigestCached _ m) = m

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

joinAllWith :: (b -> a -> b) -> b -> Joined a -> b
joinAllWith f z (Joined m) = fst (rawJoinAll f m (z, mempty))

joinAll :: BoundedJoinSemiLattice a => Joined a -> a
joinAll = joinAllWith (\/) bottom

-- | Union two 'Map's.  If the 'Map's contain conflicting elements, the list
-- of conflicting elements is returned via 'Left'.
unionEq_Map :: (Ord k, Eq a) =>
               Map k a
            -> Map k a
            -> Either [(k, a, a)] (Map k a)
unionEq_Map m1 m2
  | null conflicts = Right (m1 <> m2)
  | otherwise      = Left conflicts
  where
    conflicts = do
      (k, v1) <- Map.toAscList (Map.intersection m1 m2)
      let v2 = m2 Map.! k
      guard (v1 /= v2)
      pure (k, v1, v2)

-- Nothing -> this macro is intentionally not defined
-- Just X -> this macro must be defined to be X
data Makefile =
  Makefile
  { _rules :: !(Map FilePath ([FilePath], [String]))
  , _macros :: !(Map String (Maybe String))
  } deriving (Eq, Ord, Read, Show)

-- TODO: change value from String to Maybe String because
-- we wanna handle the case where we EXPECT the macro to be defaulted/empty

data MakefileError
  = ERuleConflict (FilePath, ([FilePath], [String]), ([FilePath], [String]))
  | EMacroConflict (String, Maybe String, Maybe String)
  deriving (Eq, Ord, Read, Show)

mempty_Makefile = Makefile mempty mempty

combineMakefile (Makefile x1 x2) (Makefile y1 y2) = do
  z1 <- left (ERuleConflict <$>) (unionEq_Map x1 y1)
  z2 <- left (EMacroConflict <$>) (unionEq_Map x2 y2)
  pure (Makefile z1 z2)

instance Monoid (Either [MakefileError] Makefile) where
  mempty = Right mempty_Makefile
  mx `mappend` my = do
    x <- mx
    y <- my
    combineMakefile x y

data Make a = Make !Makefile !a
            | MakeError ![MakefileError]
            deriving (Eq, Ord, Read, Show)

instance Functor Make where
  f `fmap` Make m x = Make m (f x)

instance Applicative Make where
  pure x = Make mempty_Makefile x
  Make mf f <*> Make mx x =
    case combineMakefile mf mx of
      Left e ->
        MakeError e
      Right my ->
        Make my (f x)

instance Monad Make where
  return = pure
  Make mx x >>= f =
    case f x of
      Make my y ->
        case combineMakefile my mx of
          Left e ->
            MakeError e
          Right myx ->
            Make myx y

-- derived from v1's simple_command
-- TODO: If output filename begins with '#', it is a phony rule
rule :: FilePath                        -- ^ output filename
     -> [String]                        -- ^ shell commands
     -> [Make FilePath]                 -- ^ dependencies
     -> Make FilePath
rule fn cmds deps = do
  depFns <- sequenceA deps
  Make (Makefile{ _rules = Map.fromList [(fn, (depFns, cmds))]
                , _macros = Map.fromList [] }) fn

newtype Digest a = Digest ByteString deriving (Eq, Ord, Show)

class Digestible a where
  rawDigest :: a -> ByteString

instance Digestible ByteString where
  rawDigest s =
    ByteArray.convert
    (CryptoHash.hash s :: CryptoHash.Digest CryptoHash.SHA1)

instance Digestible String where
  rawDigest = rawDigest . Text.pack

instance Digestible Text where
  rawDigest = rawDigest . TextEncoding.encodeUtf8

instance (Digestible a, Digestible b) => Digestible (a, b) where
  rawDigest (x, y) = rawDigest (mconcat [rawDigest x, rawDigest y])

instance Digestible a => Digestible [a] where
  rawDigest xs = rawDigest (mconcat (rawDigest <$> xs))

instance (Digestible k, Digestible a) => Digestible (Map k a) where
  rawDigest = rawDigest . Map.toAscList

digest :: Digestible a => a -> Digest a
digest = Digest . rawDigest

-- wait a second ... Map union isn't as efficient as I thought D:

------------------------------------------------------------------------------

-- | Laws are @isMempty mempty ≡ True@ and if @a ≡ t b@ and @Foldable t@ then
-- @isMempty ≡ null@.
class Monoid a => MemptyComparable a where
  isMempty :: a -> Bool

newtype VarMap
  = VarMap (Map TypeRep Dynamic)
  deriving Show

field :: (Functor f, Eq a, Monoid a, Typeable a) =>
         (a -> f a) -> VarMap -> f VarMap
field f (VarMap m) =
  case undefined of
    dummy_a ->
      let set x | x == mempty = Nothing
                | otherwise   = Just (toDyn (x `asTypeOf` dummy_a))
          get y = fromMaybe mempty (fromDynamic =<< y)
          upd y = set <$> f (get y)
      in VarMap <$> at_Map (typeOf dummy_a) upd m
