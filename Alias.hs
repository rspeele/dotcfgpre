{-# LANGUAGE OverloadedStrings #-}
module Alias
    ( AliasMap
    , AliasId
    , emptyAliasMap
    , generateAlias
    , generateDynamicAlias
    , assignDynamicAliasId
    , assignAlias
    , addAliasHookPair
    , nameAliasId
    , invokeAliasId
    , invokeDynamicAliasId
    , aliasMapCode
    ) where
import HighLevel
import LowLevel
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

type AliasId = Int

newtype DynamicAliasId = DynamicAliasId AliasId deriving (Show, Read, Eq, Ord)

data ExportAlias =
    ExportAlias
    { exportName :: AliasName
    , exportHooks :: [RawStatement]
    }

data AliasMap =
    AliasMap
    { internals :: Map [RawStatement] AliasId
    , exports :: Map AliasName ExportAlias
    , dynamics :: Map DynamicAliasId AliasId
    , maxAliasId :: Int
    }

emptyAliasMap =
    AliasMap
    { internals = M.empty
    , exports = M.empty
    , dynamics = M.empty
    , maxAliasId = 0
    }

nextAliasId :: AliasMap -> AliasId
nextAliasId = (+ 1) . maxAliasId

nameAliasId :: AliasId -> AliasName
nameAliasId id = B.concat ["_", B.pack $ show id]

nameAliasProxy :: ExportAlias -> AliasName
nameAliasProxy export = B.concat ["_", exportName export]

invokeAliasId :: AliasId -> RawStatement
invokeAliasId id = RawStatement (nameAliasId id) []

invokeDynamicAliasId :: DynamicAliasId -> RawStatement
invokeDynamicAliasId (DynamicAliasId id) = invokeAliasId id

-- | Creates/gets the alias ID for the given code.
generateAlias :: [RawStatement] -> State AliasMap AliasId
generateAlias code = do
  st <- get
  let existing = M.lookup code $ internals st
  case existing of
    Just id -> return id
    Nothing -> do
      let newId = nextAliasId st
          newMap = M.insert code newId $ internals st
      put $ st { internals = newMap
               , maxAliasId = newId }
      return newId

-- | Creates an alias ID which will be set to the given alias by default, but may be
-- dynamically reassigned during script execution.
generateDynamicAlias :: AliasId -> State AliasMap DynamicAliasId
generateDynamicAlias staticId = do
  st <- get
  let id = nextAliasId st
      dyn = DynamicAliasId id
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
      proxy <- generateAlias []
      put $ st { exports = M.insert name newEx $ exports st }
      return newEx
        where newEx = ExportAlias { exportName = name, exportHooks = [] }

assignAliasId :: AliasId -> [RawStatement] -> RawStatement
assignAliasId id code =
    RawStatement "alias" [nameAliasId id, rawsInQuotes code]

-- | Generates a statement that will reassign a dynamic alias.
assignDynamicAliasId :: DynamicAliasId -> AliasId -> RawStatement
assignDynamicAliasId (DynamicAliasId id) staticId =
    RawStatement "alias" [nameAliasId id, rawInQuotes $ invokeAliasId staticId]

-- | Returns a raw statement that will change the main definition of
-- the given name to the given code.
assignAlias :: AliasName -> [RawStatement] -> State AliasMap RawStatement
assignAlias name code = do
  codeId <- generateAlias code
  export <- exportAlias name
  return $ RawStatement "alias" [nameAliasProxy export, nameAliasId codeId]

-- | Add code to run whenever the given alias name is executed,
-- regardless of what it has been dynamically reassigned to.
addAliasHook :: AliasName -> [RawStatement] -> State AliasMap ()
addAliasHook name hook = do
  export <- exportAlias name
  hookId <- generateAlias hook
  st <- get
  let newEx = export { exportHooks = invokeAliasId hookId : exportHooks export }
  put $ st { exports = M.insert name newEx $ exports st }
  return ()

addAliasHookPair :: AliasName -> [RawStatement] -> [RawStatement] -> State AliasMap ()
addAliasHookPair name pressed released = do
  addAliasHook (B.cons '+' name) pressed
  addAliasHook (B.cons '-' name) released

exportAliasCode :: AliasMap -> [RawStatement]
exportAliasCode amap =
    concat
    [ [ RawStatement "alias" [name, rawsInQuotes $ proxy : hooks ]
      , RawStatement "alias" [proxyName, rawsInQuotes []] ]
      | (name, proxyName, proxy, hooks) <- infos ]
        where
          exps = M.toList $ exports amap
          infos = map (getInf . snd) exps
          getInf exp =
              ( exportName exp
              , proxyName
              , RawStatement proxyName []
              , exportHooks exp
              ) where proxyName = nameAliasProxy exp

internalAliasCode :: AliasMap -> [RawStatement]
internalAliasCode amap =
    [ assignAliasId id code
     | (code, id) <- M.toList $ internals amap ]

dynamicAliasCode :: AliasMap -> [RawStatement]
dynamicAliasCode amap =
    [ assignDynamicAliasId id staticId
      | (id, staticId) <- M.toList $ dynamics amap ]


-- | Get the raw code that the alias map requires for exported aliases to function.
aliasMapCode :: AliasMap -> [RawStatement]
aliasMapCode amap =
    concat
    [ dynamicAliasCode amap
    , exportAliasCode amap
    , internalAliasCode amap
    ]

