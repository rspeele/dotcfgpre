{-# LANGUAGE OverloadedStrings #-}
module Compiler
    ( compile
    ) where
import Alias
import Language
import CoreTypes
import Instruction
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

data SimpleStmt
    = Raw ByteString [ByteString]
    | Block [SimpleStmt]
    | Bind KeyName SimpleStmt
    | Alias AliasName SimpleStmt
    | IfCheck AliasName SimpleStmt SimpleStmt

expand :: SmartStmt -> SimpleStmt
expand (DirectStmt cmd args) = Raw cmd args
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

compile :: [SmartStmt] -> ([Instruction], AliasMap)
compile ss = runState (compileSimpleCode $ map expand ss) emptyAliasMap

compileSimpleCode :: [SimpleStmt] -> State AliasMap [Instruction]
compileSimpleCode ss = mapM compileSimple ss >>= return . concat

compileSimple :: SimpleStmt -> State AliasMap [Instruction]
compileSimple s =
    case s of
      Raw cmd args -> return $ [InvokeRaw cmd args]
      Block block -> compileBlock block
      Alias name body -> compileAlias name body
      Bind key stmt -> compileBind key stmt
      IfCheck name pos neg -> compileIf name pos neg

compileBlock :: [SimpleStmt] -> State AliasMap [Instruction]
compileBlock block = do
  compiled <- compileSimpleCode block
  aliasId <- generateAlias compiled
  return $ [InvokeStatic aliasId]

compileAlias :: AliasName -> SimpleStmt -> State AliasMap [Instruction]
compileAlias name body = do
  code <- compileSimple body
  assignmentCode <- assignAlias name code
  return [assignmentCode]

compileBind :: KeyName -> SimpleStmt -> State AliasMap [Instruction]
compileBind key body = do
  code <- compileSimple body
  return [BindKey key code]

compileIf :: AliasName -> SimpleStmt -> SimpleStmt -> State AliasMap [Instruction]
compileIf alias pos neg = do
  negCode <- compileSimple neg
  posCode <- compileSimple pos
  pressedAliasId <- generateAlias posCode
  releasedAliasId <- generateAlias negCode
  switchAliasId <- generateDynamicAlias releasedAliasId
  addAliasHookPair alias
                       [AssignDynamic switchAliasId $ InvokeStatic pressedAliasId]
                       [AssignDynamic switchAliasId $ InvokeStatic releasedAliasId]
  return [InvokeDynamic switchAliasId]

