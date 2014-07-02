{-# LANGUAGE OverloadedStrings #-}
module Alias
    ( AliasMap
    , emptyAliasMap
    , generateAlias
    , generateDynamicAlias
    , assignAlias
    , addAliasHookPair
    , aliasMapCode
    ) where
import CoreTypes
import Instruction
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

data ExportAlias =
    ExportAlias
    { exportName :: AliasName
    , exportProxy :: DynamicAlias
    , exportHooks :: [Instruction]
    }

data AliasMap =
    AliasMap
    { internals :: Map [Instruction] StaticAlias
    , exports :: Map AliasName ExportAlias
    , dynamics :: Map DynamicAlias StaticAlias
    , maxAliasId :: Int
    }

emptyAliasMap =
    AliasMap
    { internals = M.empty
    , exports = M.empty
    , dynamics = M.empty
    , maxAliasId = 0
    }

nextAliasId :: AliasMap -> Int
nextAliasId = (+ 1) . maxAliasId

-- | Creates/gets the alias ID for the given code.
generateAlias :: [Instruction] -> State AliasMap StaticAlias
generateAlias [InvokeStatic st] = return st
generateAlias code = do
  st <- get
  let existing = M.lookup code $ internals st
  case existing of
    Just id -> return id
    Nothing -> do
      let newId = nextAliasId st
          newStatic = StaticAlias newId
          newMap = M.insert code newStatic $ internals st
      put $ st { internals = newMap
               , maxAliasId = newId }
      return newStatic

-- | Creates an alias ID which will be set to the given alias by default, but may be
-- dynamically reassigned during script execution.
generateDynamicAlias :: StaticAlias -> State AliasMap DynamicAlias
generateDynamicAlias staticId = do
  st <- get
  let id = nextAliasId st
      dyn = DynamicAlias id
  put $ st { maxAliasId = id, dynamics = M.insert dyn staticId $ dynamics st }
  return dyn

-- | Creates/gets the export alias for the given alias name.
exportAlias :: AliasName -> State AliasMap ExportAlias
exportAlias name = do
  st <- get
  let existing = M.lookup name $ exports st
  case existing of
    Just e -> return e
    Nothing -> do
      proxy <- generateAlias [] >>= generateDynamicAlias
      let newEx =
              ExportAlias
              { exportName = name
              , exportProxy = proxy
              , exportHooks = [] }
      put $ st { exports = M.insert name newEx $ exports st }
      return newEx

-- | Returns a raw statement that will change the main definition of
-- the given name to the given code.
assignAlias :: AliasName -> [Instruction] -> State AliasMap Instruction
assignAlias name code = do
  codeId <- generateAlias code
  export <- exportAlias name
  return $ AssignDynamic (exportProxy export) $ InvokeStatic codeId

-- | Add code to run whenever the given alias name is executed,
-- regardless of what it has been dynamically reassigned to.
addAliasHook :: AliasName -> [Instruction] -> State AliasMap ()
addAliasHook name hook = do
  export <- exportAlias name
  hookId <- generateAlias hook
  st <- get
  let newEx = export { exportHooks = InvokeStatic hookId : exportHooks export }
  put $ st { exports = M.insert name newEx $ exports st }
  return ()

addAliasHookPair :: AliasName -> [Instruction] -> [Instruction] -> State AliasMap ()
addAliasHookPair name pressed released = do
  addAliasHook (B.cons '+' name) pressed
  addAliasHook (B.cons '-' name) released

exportAliasCode :: AliasMap -> [Instruction]
exportAliasCode amap =
    [ AssignName name (proxy : hooks)
      | (name, proxy, hooks) <- infos ]
        where
          exps = M.toList $ exports amap
          infos = map (getInf . snd) exps
          getInf exp =
              ( exportName exp
              , InvokeDynamic $ exportProxy exp
              , exportHooks exp
              )

internalAliasCode :: AliasMap -> [Instruction]
internalAliasCode amap =
    [ AssignStatic id code
     | (code, id) <- M.toList $ internals amap ]

dynamicAliasCode :: AliasMap -> [Instruction]
dynamicAliasCode amap =
    [ AssignDynamic id $ InvokeStatic staticId
      | (id, staticId) <- M.toList $ dynamics amap ]

-- | Get the raw code that the alias map requires for exported aliases to function.
aliasMapCode :: AliasMap -> [Instruction]
aliasMapCode amap =
    concat
    [ dynamicAliasCode amap
    , exportAliasCode amap
    , internalAliasCode amap
    ]

