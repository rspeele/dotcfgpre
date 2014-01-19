{-# LANGUAGE OverloadedStrings #-}
module Compilation where
import HighLevel
import LowLevel
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

type AliasId = Int

data ExportAlias
    = ExportAlias
      { aliasCode :: [RawStatement]
      , aliasExtra :: [RawStatement]
      } deriving (Show, Read, Eq)

data Compilation
    = Compilation
      { genAliases :: Map AliasId [RawStatement]
      , expAliases :: Map AliasName ExportAlias
      } deriving (Show, Read, Eq)

assembleAlias :: AliasName -> [RawStatement] -> ByteString
assembleAlias sym code =
    B.concat ["alias ", sym, " \"", rawsInQuotes code, "\""]

assembleGeneratedAliases :: Map AliasId [RawStatement] -> ByteString
assembleGeneratedAliases m =
    joinBy '\n' aliases
    where
      nameGen (a, rs) = (nameAliasId a, rs)
      gens = map nameGen $ M.toList $ m
      aliases = map (uncurry assembleAlias) gens

assembleExportAlias :: AliasName -> ExportAlias -> ByteString
assembleExportAlias sym exp = assembleAlias sym allCode
    where
      allCode = concat [aliasCode exp, aliasExtra exp]

assembleExportAliases :: Map AliasName ExportAlias -> ByteString
assembleExportAliases m =
    joinBy '\n' aliases
    where
      exps = M.toList m
      aliases = map (uncurry assembleExportAlias) exps

assembleCompilation :: Compilation -> ByteString
assembleCompilation cmp =
    joinBy '\n' [exportCode, generatedCode]
    where
      exportCode = assembleExportAliases $ expAliases cmp
      generatedCode = assembleGeneratedAliases $ genAliases cmp

assemble :: [RawStatement] -> Compilation -> ByteString
assemble code cmp = B.concat [assembleCompilation cmp, rawsTopLevel code]

emptyCompilation :: Compilation
emptyCompilation =
    Compilation
    { genAliases = M.empty
    , expAliases = M.empty
    }

nextAliasId :: Map AliasId a -> AliasId
nextAliasId m
    | M.null m = 0
    | otherwise = 1 + (fst $ M.findMax m)

nameAliasId :: AliasId -> AliasName
nameAliasId id = B.concat [":", B.pack $ show id]

invokeAliasId :: AliasId -> RawStatement
invokeAliasId id = RawStatement (nameAliasId id) []

generateAlias :: [RawStatement] -> State Compilation AliasId
generateAlias code = do
  st <- get
  let oldMap = genAliases st
      newId = nextAliasId oldMap
      newMap = M.insert newId code oldMap
  put $ st { genAliases = newMap }
  return newId

exportAlias :: AliasName -> [RawStatement] -> State Compilation AliasId
exportAlias name code = do
  st <- get
  let oldMap = expAliases st
      alias = ExportAlias { aliasCode = code, aliasExtra = [] }
      rcode new old =
          ExportAlias
          { aliasCode = aliasCode new
          , aliasExtra = aliasExtra new ++ aliasExtra old
          }
      newMap = M.insertWith rcode name alias oldMap
  put $ st { expAliases = newMap }
