module Data.GH3.QB where

import           Data.Bits(complement)
import           Data.ByteString(ByteString)
import           Data.Digest.CRC32(crc32)
import qualified Data.Map.Strict as M
import           Data.String(fromString)
import           Data.Word(Word32)

data QbType = QbType deriving (Show, Eq)

data QbKey = QbCrc Word32
           | QbName String
           deriving (Show, Eq, Ord)

data QbValue = QbValue deriving (Show, Eq)

data Struct = Struct [StructItem]
            deriving (Show, Eq)

data StructItem = StructItem QbType QbKey QbValue
                deriving (Show, Eq)

canonicalise :: QbKey -> QbKey
canonicalise (QbCrc x) = QbCrc x
canonicalise (QbName s) = QbCrc . checksum . fmap subst $ s
  where
    subst x = M.findWithDefault x x substitutions

    substitutions = M.fromList $ [('/', '\\')] ++ zip ['A'..'Z'] ['a'..'z']

checksum :: String -> Word32
checksum s = complement . crc32 $ (fromString s :: ByteString)
