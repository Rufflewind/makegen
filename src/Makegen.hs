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
import Data.Monoid
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Makegen.Internal.Digest

-- | Equivalent to @Control.Lens.At.at@ for 'Map'.
at_Map :: (Functor f, Ord k) =>
          k -> (Maybe a -> f (Maybe a)) -> Map k a -> f (Map k a)
at_Map k f m = set <$> f (Map.lookup k m)
  where
    set Nothing  = Map.delete k m
    set (Just x) = Map.insert k x m

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
rawRule :: FilePath                     -- ^ output filename
        -> [String]                     -- ^ shell commands
        -> [FilePath]                   -- ^ dependencies
        -> Make FilePath
rawRule fn cmds depFns =
  Make (Makefile{ _rules = Map.fromList [(fn, (depFns, cmds))]
                , _macros = Map.fromList [] }) fn

-- derived from v1's simple_command
rule :: FilePath                        -- ^ output filename
     -> [String]                        -- ^ shell commands
     -> [Make FilePath]                 -- ^ dependencies
     -> Make FilePath
rule fn cmds deps = rawRule fn cmds =<< sequenceA deps
