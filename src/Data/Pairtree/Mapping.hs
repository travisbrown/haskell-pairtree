module Data.Pairtree.Mapping
  ( clean
  , unclean
  ) where
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import Data.Char (chr, digitToInt)
import Data.Monoid (mappend)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Format (left)
import Data.Text.Format.Types (Hex (Hex))
import qualified Data.Text.Lazy.Builder as TB (singleton, toLazyText)
import Data.Word (Word8)

problemChar :: Word8 -> Bool
problemChar 0x22 = True
problemChar 0x2a = True
problemChar 0x2b = True
problemChar 0x2c = True
problemChar 0x3c = True
problemChar 0x3d = True
problemChar 0x3e = True
problemChar 0x3f = True
problemChar 0x5c = True
problemChar 0x5e = True
problemChar 0x7c = True
problemChar w = w < 0x21 || w > 0x7e

hexEncode :: Word8 -> ByteString
hexEncode
  = encodeUtf8
  . TB.toLazyText
  . (mappend $ TB.singleton '^')
  . left 2 '0'
  . Hex

clean :: Text -> Text
clean = decodeUtf8 . B.concatMap escapeIfNeeded . encodeUtf8
  where
    escapeIfNeeded w
      | problemChar w = hexEncode w
      | w == 0x2f = B.singleton 0x3d
      | w == 0x3a = B.singleton 0x2b
      | w == 0x2e = B.singleton 0x2c
      | otherwise     = B.singleton w

data State = Plain | Escaped | First Word8

unclean :: Text -> Text
unclean = decodeUtf8 . snd . B.foldl unescape (Plain, B.empty) . encodeUtf8
  where
    unescape (Escaped, current) w = (First $ asHex1 w, current) 
    unescape (First p, current) w = (Plain, B.snoc current $ p + asHex2 w)
    unescape (Plain, current) w
      | w == 0x5e = (Escaped, current)
      | otherwise = (Plain, B.snoc current w)

    asHex1 = (16 *) . fromIntegral . digitToInt . chr . fromIntegral
    asHex2 = fromIntegral . digitToInt . chr . fromIntegral

