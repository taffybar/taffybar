{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Util.ReadByteString (parseInt) where

import Data.ByteString (ByteString())
import Data.Text (Text())
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)

--partial
fromReader :: Either String (a, Text) -> a
fromReader (Right (n, "")) = n

parseInt :: ByteString -> Int
parseInt = fromReader . decimal . decodeUtf8
