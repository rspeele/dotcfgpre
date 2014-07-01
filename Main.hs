{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language
import Parser
import Compiler
import Alias
import RawCfg
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

assemble :: [RawStmt] -> AliasMap -> ByteString
assemble code cmp =
    B.concat
         [ rawsTopLevel $ aliasMapCode cmp
         , "\n\n\n"
         , rawsTopLevel code
         ]

main :: IO ()
main = do
  contents <- B.readFile "conditions.xcfg"
  let parseResult = parseStmts contents
  case parseResult of
    Left error -> print error
    Right parsed ->
        do
          let (code, cmp) = compile parsed
              assy = assemble code cmp
          B.putStrLn assy
