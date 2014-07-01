module Language where
import RawCfg
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

type KeyName = ByteString
type AliasName = ByteString

data Condition
    = Check AliasName
    | Not Condition
    | Or Condition Condition
    | And Condition Condition
      deriving (Show, Read, Eq)

pressed = Check
released = Not . Check

oppositeCondition :: Condition -> Condition
oppositeCondition c = Not c

data SmartStmt
    = DirectStmt RawStmt
    | IfStmt Condition SmartStmt SmartStmt
    | BlockStmt [SmartStmt]
    | BindStmt KeyName SmartStmt
    | AliasStmt AliasName SmartStmt
      deriving (Show, Read, Eq)

