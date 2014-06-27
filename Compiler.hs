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

compileCode :: [Statement] -> State AliasMap [RawStatement]
compileCode ss = mapM compileStatement ss >>= return . concat

compileStatement :: Statement -> State AliasMap [RawStatement]
compileStatement s =
    case s of
      Raw raw -> compileRaw raw
      Block block -> compileBlock block
      Alias name body -> compileAlias name body
      Bind key stmt -> compileBind key stmt
      If cond pos neg -> compileIf cond pos neg

compileRaw :: RawStatement -> State AliasMap [RawStatement]
compileRaw = return . return

compileBlock :: [Statement] -> State AliasMap [RawStatement]
compileBlock block = do
  compiled <- compileCode block
  aliasId <- generateAlias compiled
  return $ [invokeAliasId aliasId]

compileAlias :: AliasName -> Statement -> State AliasMap [RawStatement]
compileAlias name body = do
  code <- compileStatement body
  assignmentCode <- assignAlias name code
  return [assignmentCode]

compileBind :: KeyName -> Statement -> State AliasMap [RawStatement]
compileBind key body = do
  code <- compileStatement body
  return [RawStatement "bind" [ key, rawsInQuotes code ]]

compileIf :: Condition -> Statement -> Maybe Statement -> State AliasMap [RawStatement]
compileIf (Condition b alias) pos neg = do
  negCode <- case neg of
               Nothing -> return []
               Just negSt -> compileStatement negSt
  posCode <- compileStatement pos
  let (pressedCode, releasedCode) =
          if b then (posCode, negCode) else (negCode, posCode)
  pressedAliasId <- generateAlias pressedCode
  releasedAliasId <- generateAlias releasedCode
  switchAliasId <- generateDynamicAlias releasedAliasId
  addAliasHookPair alias
                       [assignDynamicAliasId switchAliasId pressedAliasId]
                       [assignDynamicAliasId switchAliasId releasedAliasId]
  return [invokeDynamicAliasId switchAliasId]

compile :: [Statement] -> ([RawStatement], AliasMap)
compile ss = runState (compileCode ss) emptyAliasMap
