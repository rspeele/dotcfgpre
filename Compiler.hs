module Compiler where
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
nameAliasId id = B.pack $ concat [":", show id]

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

exportAlias :: AliasName -> [RawStatement] -> State Compilation ()
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
compileRaw = return . (: [])

compileBlock :: [Statement] -> State Compilation [RawStatement]
compileBlock block = do
  compiled <- compileCode block
  aliasId <- generateAlias compiled
  return $ [invokeAliasId aliasId]

compileAlias :: AliasName -> Statement -> State Compilation [RawStatement]
compileAlias name body = do
  code <- compileStatement body
  name <- exportAlias name code
  return []

compileBind :: KeyName -> Statement -> State Compilation [RawStatement]
compileBind key body = undefined

compile :: [Statement] -> ([RawStatement], Compilation)
compile ss = runState (compileCode ss) emptyCompilation


