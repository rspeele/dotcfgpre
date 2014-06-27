{-# LANGUAGE OverloadedStrings #-}
module LowLevel where
import Data.List (intersperse)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

joinBy :: Char -> [ByteString] -> ByteString
joinBy c bs  = B.concat $ intersperse (B.singleton c) bs

wrapWith :: Char -> ByteString -> ByteString
wrapWith c s = B.concat [B.singleton c, s, B.singleton c]

data RawStatement
    = RawStatement ByteString [ByteString]
      deriving (Show, Read, Eq, Ord)

rawTopLevel :: RawStatement -> ByteString
rawTopLevel (RawStatement sym args)
    = joinBy ' ' $ sym : map (wrapWith '"') args

rawsTopLevel :: [RawStatement] -> ByteString
rawsTopLevel = joinBy '\n' . map rawTopLevel

rawInQuotes :: RawStatement -> ByteString
rawInQuotes (RawStatement sym args)
    = joinBy ' ' $ sym : args

rawsInQuotes :: [RawStatement] -> ByteString
rawsInQuotes = joinBy ';' . map rawInQuotes


