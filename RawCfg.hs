{-# LANGUAGE OverloadedStrings #-}
module RawCfg where
import Data.List (intersperse)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

joinBy :: Char -> [ByteString] -> ByteString
joinBy c bs  = B.concat $ intersperse (B.singleton c) bs

wrapWith :: Char -> ByteString -> ByteString
wrapWith c s = B.concat [B.singleton c, s, B.singleton c]

data RawStmt
    = RawStmt ByteString [ByteString]
      deriving (Show, Read, Eq, Ord)

rawTopLevel :: RawStmt -> ByteString
rawTopLevel (RawStmt sym args)
    = joinBy ' ' $ sym : map (wrapWith '"') args

rawsTopLevel :: [RawStmt] -> ByteString
rawsTopLevel = joinBy '\n' . map rawTopLevel

rawInQuotes :: RawStmt -> ByteString
rawInQuotes (RawStmt sym args)
    = joinBy ' ' $ sym : args

rawsInQuotes :: [RawStmt] -> ByteString
rawsInQuotes = joinBy ';' . map rawInQuotes


