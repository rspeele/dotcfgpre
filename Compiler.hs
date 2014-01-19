module Compiler where
import Compilation
import HighLevel
import LowLevel
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

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
  name <- exportAlias name code -- TODO: what if this is a dynamic re-aliasing?
  return []
-- Shouldn't this get an alias name to perform the
-- re-aliasing, which it then invokes? Maybe not, because
-- it might not be possible to do the re-aliasing through
-- an alias. Actually, it is. When you run exportAlias x
-- "say hello", you should really get three things:
-- 1. :0 is genAlias "say hello"
-- 2. Put x into expAlises as ":0"
-- 3. Make a genAlias "alias x :0"
-- 4. Invoke that genAlias as code.
-- So what if "alias x" shows up a lot? What is the end state of the expalias?
-- It will work out OK, because the last top-level statement should always win.
-- And that is what will happen, since the last top-level statement
-- will in fact invoke its own assignment.
-- But wait!
-- That's not entirely true...
-- All of the extraCode in the expAlias, generated from things
-- like "if *x", needs to apply for *any* value of x's alias assignment.
-- One approach would be to put that code in all the genAliases made
-- in the step 1. above. But that would be gratuitous and hard to
-- guarantee. So maybe the right flow is to use one more proxy genAlias
-- which will represent x's current code assignement.
-- The output should look like:
-- alias x "xextra1;xextra2;proxy_x"
-- The [RawStatement] spit out for a compileAlias would actually be a re-alias of proxy_x.

compileBind :: KeyName -> Statement -> State Compilation [RawStatement]
compileBind key body = undefined

compile :: [Statement] -> ([RawStatement], Compilation)
compile ss = runState (compileCode ss) emptyCompilation


