{-# LANGUAGE OverloadedStrings #-}
module Compiler
    ( compile
    ) where
import Alias
import Language
import RawCfg
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

data SimpleStmt
    = Raw RawStmt
    | Block [SimpleStmt]
    | Bind KeyName SimpleStmt
    | Alias AliasName SimpleStmt
    | IfCheck AliasName SimpleStmt SimpleStmt

expand :: SmartStmt -> SimpleStmt
expand (DirectStmt code) = Raw code
expand (BlockStmt block) = Block $ map expand block
expand (BindStmt key st) = Bind key $ expand st
expand (AliasStmt alias st) = Alias alias $ expand st
expand (IfStmt cond ifTrue ifFalse) =
    case cond of
      Check vname -> IfCheck vname (expand ifTrue) (expand ifFalse)
      Not ncond -> expand $ IfStmt ncond ifFalse ifTrue
      And lcond rcond ->
          let inner = IfStmt rcond ifTrue ifFalse
          in expand $ IfStmt lcond inner ifFalse
      Or lcond rcond ->
          let second = IfStmt rcond ifTrue ifFalse
          in expand $ IfStmt lcond ifTrue second

compile :: [SmartStmt] -> ([RawStmt], AliasMap)
compile ss = runState (compileSimpleCode $ map expand ss) emptyAliasMap

compileSimpleCode :: [SimpleStmt] -> State AliasMap [RawStmt]
compileSimpleCode ss = mapM compileSimple ss >>= return . concat

compileSimple :: SimpleStmt -> State AliasMap [RawStmt]
compileSimple s =
    case s of
      Raw raw -> compileRaw raw
      Block block -> compileBlock block
      Alias name body -> compileAlias name body
      Bind key stmt -> compileBind key stmt
      IfCheck name pos neg -> compileIf name pos neg

compileRaw :: RawStmt -> State AliasMap [RawStmt]
compileRaw = return . return

compileBlock :: [SimpleStmt] -> State AliasMap [RawStmt]
compileBlock block = do
  compiled <- compileSimpleCode block
  aliasId <- generateAlias compiled
  return $ [invokeAliasId aliasId]

compileAlias :: AliasName -> SimpleStmt -> State AliasMap [RawStmt]
compileAlias name body = do
  code <- compileSimple body
  assignmentCode <- assignAlias name code
  return [assignmentCode]

compileBind :: KeyName -> SimpleStmt -> State AliasMap [RawStmt]
compileBind key body = do
  code <- compileSimple body
  return [RawStmt "bind" [ key, rawsInQuotes code ]]

compileIf :: AliasName -> SimpleStmt -> SimpleStmt -> State AliasMap [RawStmt]
compileIf alias pos neg = do
  negCode <- compileSimple neg
  posCode <- compileSimple pos
  pressedAliasId <- generateAlias posCode
  releasedAliasId <- generateAlias negCode
  switchAliasId <- generateDynamicAlias releasedAliasId
  addAliasHookPair alias
                       [assignDynamicAliasId switchAliasId pressedAliasId]
                       [assignDynamicAliasId switchAliasId releasedAliasId]
  return [invokeDynamicAliasId switchAliasId]

