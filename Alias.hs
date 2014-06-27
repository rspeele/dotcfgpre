{-# LANGUAGE OverloadedStrings #-}
module Alias
    ( AliasMap
    , AliasId
    , emptyAliasMap
    , generateAlias
    , assignAlias
    , addAliasHook
    , nameAliasId
    , invokeAliasId
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

data ExportAlias =
    ExportAlias
    { exportName :: AliasName
    , exportHooks :: [RawStatement]
    }

data AliasMap =
    AliasMap
    { internals :: Map [RawStatement] AliasId
    , exports :: Map AliasName ExportAlias
    , maxAliasId :: Int
    }

emptyAliasMap =
    AliasMap
    { internals = M.empty
    , exports = M.empty
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

exportAliasCode :: AliasMap -> [RawStatement]
exportAliasCode amap =
    [ RawStatement "alias" [name, rawsInQuotes $ proxy : hooks]
      | (name, proxy, hooks) <- infos ]
        where
          exps = M.toList $ exports amap
          infos = map (getInf . snd) exps
          getInf exp =
              ( exportName exp
              , RawStatement (nameAliasProxy exp) []
              , exportHooks exp
              )

internalAliasCode :: AliasMap -> [RawStatement]
internalAliasCode amap =
    [ RawStatement "alias" [nameAliasId id, rawsInQuotes code]
     | (code, id) <- M.toList $ internals amap ]

-- | Get the raw code that the alias map requires for exported aliases to function.
aliasMapCode :: AliasMap -> [RawStatement]
aliasMapCode map = exportAliasCode map ++ internalAliasCode map

