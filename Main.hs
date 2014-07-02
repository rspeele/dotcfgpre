{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language
import Parser
import Compiler
import Alias
import Instruction
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

build :: [Instruction] -> AliasMap -> ByteString
build code cmp =
    B.concat
         [ assembleMany $ aliasMapCode cmp
         , "\n\n\n"
         , assembleMany code
         ]

main :: IO ()
main = do
  contents <- B.readFile "nullcancel.xcfg"
  let parseResult = parseStmts contents
  case parseResult of
    Left error -> print error
    Right parsed ->
        do
          let (code, cmp) = compile parsed
              built = build code cmp
          B.putStrLn built
