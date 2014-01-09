{-# LANGUAGE OverloadedStrings #-}

module LowLevel where
import Data.List (intersperse)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

joinBy :: Char -> [String] -> String
joinBy c = concat . intersperse [c]

wrapWith :: Char -> String -> String
wrapWith c s = c : s ++ [c]

data RawStatement
    = RawStatement ByteString [ByteString]
      deriving (Show, Read, Eq)

assembleOne (RawStatement sym args)
    = joinBy ' ' $ (B.unpack sym) : map (wrapWith '"' . B.unpack) args


assembleMany stmts
        = joinBy '\n' $ map assembleOne stmts

alias :: ByteString -> ByteString -> RawStatement
alias name value = RawStatement "alias" [name, value]

