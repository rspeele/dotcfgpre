module HighLevel where
import LowLevel
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

type KeyName = ByteString
type AliasName = ByteString

data Condition
    = Plus AliasName
    | Minus AliasName
      deriving (Show, Read, Eq)

data Statement
    = Raw RawStatement
    | Block [Statement]
    | Bind KeyName Statement
    | Alias AliasName Statement
    | If Condition Statement (Maybe Statement)
      deriving (Show, Read, Eq)

