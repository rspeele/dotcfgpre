{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserGuarantees where
import Guarantee
import Parser
import LowLevel
import HighLevel
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Text.Parsec.ByteString
import Text.Parsec hiding ((<|>), many)

mustParse raw "say hello" $ Raw (RawStatement "say" ["hello"])

mustParse identifier "ab77D_92" "ab77D_92"

mustParse terminator ";" ()

mustParse terminators "; \r\n  ; ;;;" ()

mustNotParse terminators "" ()

mustNotParse terminators "  " ()

mustParse ignoredSpace "" ()

mustParse ignoredSpace " \t\r\n;" ()

mustParse requiredSpace " \t" ()

mustNotParse requiredSpace "" ()

mustNotParse requiredSpace ";" ()

mustParse quotedArgument "\"\"" ""

mustParse quotedArgument "\"this is a test; so is this\"" "this is a test; so is this"

mustNotParse quotedArgument "" ""

mustParse statements "" []

mustParse statements "\r\n   \t\n;;;" []

mustParse statements "say hello; say goodbye      "
              $ [Raw $ RawStatement "say" ["hello"], Raw $ RawStatement "say" ["goodbye"]]

mustNotParse block "" $ Block []

mustParse block "[]" $ Block []

mustParse block "[say hello; say goodbye]"
              $ Block [Raw $ RawStatement "say" ["hello"], Raw $ RawStatement "say" ["goodbye"]]

mustParse block "[say  hello  \n\n\n     say goodbye  ]"
              $ Block [Raw $ RawStatement "say" ["hello"], Raw $ RawStatement "say" ["goodbye"]]

mustParse alias "alias x say hello"
              $ Alias "x" (Raw $ RawStatement "say" ["hello"])

mustParse alias "alias x [say hello]"
              $ Alias "x" $ Block [(Raw $ RawStatement "say" ["hello"])]

