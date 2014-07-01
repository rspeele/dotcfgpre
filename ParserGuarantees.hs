{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserGuarantees where
import Guarantee
import Parser
import RawCfg
import Language
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Text.Parsec.ByteString
import Text.Parsec hiding ((<|>), many)

mustParse raw "say hello" $ DirectStmt (RawStmt "say" ["hello"])

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
              $ [DirectStmt $ RawStmt "say" ["hello"], DirectStmt $ RawStmt "say" ["goodbye"]]

mustNotParse block "" $ BlockStmt []

mustParse block "[]" $ BlockStmt []

mustParse block "[say hello; say goodbye]"
              $ BlockStmt [DirectStmt $ RawStmt "say" ["hello"], DirectStmt $ RawStmt "say" ["goodbye"]]

mustParse block "[say  hello  \n\n\n     say goodbye  ]"
              $ BlockStmt [DirectStmt $ RawStmt "say" ["hello"], DirectStmt $ RawStmt "say" ["goodbye"]]

mustParse alias "alias x say hello"
              $ AliasStmt "x" (DirectStmt $ RawStmt "say" ["hello"])

mustParse alias "alias x [say hello]"
              $ AliasStmt "x" $ BlockStmt [(DirectStmt $ RawStmt "say" ["hello"])]

mustParse condition "+x" (Check "x")
mustParse condition "(+y)" (Check "y")
mustParse condition "( +y  )" (Check "y")
mustParse condition "+x or +y" (Or (Check "x") (Check "y"))

mustParse ifSt "if +x then [say hello] else say goodbye"
              $ IfStmt (Check "x")
                    (BlockStmt [DirectStmt $ RawStmt "say" ["hello"]])
                    (DirectStmt $ RawStmt "say" ["goodbye"])

mustParse ifSt "if -x or +y then say hello and stuff"
              $ IfStmt ((Not $ Check "x") `Or` (Check "y"))
                    (DirectStmt $ RawStmt "say" ["hello", "and", "stuff"])
                    (BlockStmt [])

mustParse ifSt "if -x then say hello and stuff"
              $ IfStmt (Not $ Check "x")
                    (DirectStmt $ RawStmt "say" ["hello", "and", "stuff"])
                    (BlockStmt [])
