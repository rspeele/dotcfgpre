module Language where
import CoreTypes

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
    = DirectStmt ByteString [ByteString]
    | IfStmt Condition SmartStmt SmartStmt
    | BlockStmt [SmartStmt]
    | BindStmt KeyName SmartStmt
    | AliasStmt AliasName SmartStmt
      deriving (Show, Read, Eq)

