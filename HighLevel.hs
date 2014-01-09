module HighLevel where
import LowLevel

data Statement
    = Raw RawStatement
    | Block [Statement]
      deriving (Show, Read, Eq)

