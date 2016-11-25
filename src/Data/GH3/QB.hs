module Data.GH3.QB where

import           Data.Bits(complement)
import           Data.ByteString(ByteString)
import           Data.Digest.CRC32(crc32)
import           Data.Int(Int32)
import qualified Data.Map.Strict as M
import           Data.String(fromString)
import           Data.String.Unicode(UString)
import           Data.Word(Word32)

data QbType = QbTInteger
            | QbTFloat
            | QbTString
            | QbTWString
            | QbTVector2
            | QbTVector3
            | QbTStruct
            | QbTArray
            | QbTKey
            | QbTKeyRef -- ^ ?? From Nanook's Queen Bee source code
            | QbTStringPointer -- ^ ??
            | QbTStringQs -- ^ ?? Unknown function
            deriving (Show, Eq)

data QbKey = QbCrc Word32
           | QbName String
           deriving (Show, Eq, Ord)

data QbValue = QbInteger Int32
             | QbFloat Float
             | QbString String
             | QbWString UString
             | QbVector2 Float Float
             | QbVector3 Float Float Float
             | QbStruct Struct
             | QbArray QbArray
             | QbKey QbKey
             | QbKeyRef QbKey
             | QbStringPointer QbKey
             | QbStringQs QbKey
             deriving (Show, Eq)

data Struct = Struct [StructItem]
            deriving (Show, Eq)

data StructItem = StructItem QbType QbKey QbValue
                deriving (Show, Eq)

data QbArray = QbArr QbType [QbValue] deriving (Show, Eq)

canonicalise :: QbKey -> QbKey
canonicalise (QbCrc x) = QbCrc x
canonicalise (QbName s) = QbCrc . crc . fmap subst $ s
  where
    subst x = M.findWithDefault x x substitutions

    substitutions = M.fromList $ [('/', '\\')] ++ zip ['A'..'Z'] ['a'..'z']

crc :: String -> Word32
crc s = complement . crc32 $ (fromString s :: ByteString)
