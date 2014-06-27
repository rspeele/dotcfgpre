{-# LANGUAGE OverloadedStrings #-}
module Compiler
    ( compileCode
    , compileStatement
    , compile
    ) where
import Alias
import HighLevel
import LowLevel
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

type Compilation = AliasMap

compileCode :: [Statement] -> State Compilation [RawStatement]
compileCode ss = mapM compileStatement ss >>= return . concat

compileStatement :: Statement -> State Compilation [RawStatement]
compileStatement s =
    case s of
      Raw raw -> compileRaw raw
      Block block -> compileBlock block
      Alias name body -> compileAlias name body
      Bind key stmt -> compileBind key stmt

compileRaw :: RawStatement -> State Compilation [RawStatement]
compileRaw = return . return

compileBlock :: [Statement] -> State Compilation [RawStatement]
compileBlock block = do
  compiled <- compileCode block
  aliasId <- generateAlias compiled
  return $ [invokeAliasId aliasId]

compileAlias :: AliasName -> Statement -> State Compilation [RawStatement]
compileAlias name body = do
  code <- compileStatement body
  assignmentCode <- assignAlias name code
  return [assignmentCode]

compileBind :: KeyName -> Statement -> State Compilation [RawStatement]
compileBind key body = do
  code <- compileStatement body
  return [RawStatement "bind" [ key, rawsInQuotes code ]]

compile :: [Statement] -> ([RawStatement], Compilation)
compile ss = runState (compileCode ss) emptyAliasMap
