module Main where
import Parser
import HighLevel
import LowLevel
import Compiler
import Compilation
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  contents <- B.readFile "input.txt"
  let parseResult = parseStatements contents
  case parseResult of
    Left error -> print error
    Right parsed ->
        do
          let (code, cmp) = compile parsed
              assy = assemble code cmp
          B.putStr assy
