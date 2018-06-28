module IO where

import GHC.Word (Word8)
import qualified Data.ByteString.Lazy as BL

writeBytes :: String -> [Word8] -> IO ()
writeBytes fp s = BL.writeFile fp (BL.pack s)

readBytes :: String -> IO [Word8]
readBytes fp = BL.unpack <$> BL.readFile fp
