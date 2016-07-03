{-# LANGUAGE FlexibleInstances #-}
module Makegen.Internal.Digest
  ( digest
  , Digest(unDigest)
  , Digestible(rawDigest)
  ) where
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import qualified Crypto.Hash
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding

digest :: Digestible a => a -> Digest a
digest = Digest . rawDigest

newtype Digest a
  = Digest { unDigest :: ByteString }
  deriving (Eq, Ord, Show)

class Digestible a where
  rawDigest :: a -> ByteString

instance Digestible ByteString where
  rawDigest s =
    ByteArray.convert
    (Crypto.Hash.hash s :: Crypto.Hash.Digest Crypto.Hash.SHA1)

instance Digestible String where
  rawDigest = rawDigest . Text.pack

instance Digestible Text where
  rawDigest = rawDigest . Text.Encoding.encodeUtf8

instance (Digestible a, Digestible b) => Digestible (a, b) where
  rawDigest (x, y) = rawDigest (mconcat [rawDigest x, rawDigest y])

instance Digestible a => Digestible [a] where
  rawDigest xs = rawDigest (mconcat (rawDigest <$> xs))

instance (Digestible k, Digestible a) => Digestible (Map k a) where
  rawDigest = rawDigest . Map.toAscList
