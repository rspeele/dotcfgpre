module HighLevel where
import LowLevel
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

type KeyName = ByteString
type AliasName = ByteString

data Statement
    = Raw RawStatement
    | Block [Statement]
    | Bind KeyName Statement
    | Alias AliasName Statement
      deriving (Show, Read, Eq)

