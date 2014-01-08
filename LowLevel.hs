{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module LowLevel where
import Data.List (intersperse)
import Text.Parsec

joinBy :: Char -> [String] -> String
joinBy c = concat . intersperse [c]

wrapWith :: Char -> String -> String
wrapWith c s = c : s ++ [c]

class Assemble a b where
    assemble :: a -> b

type RawCode = [RawStatement]

data RawStatement
    = RawStatement String [String]
      deriving (Show, Read, Eq)

instance Assemble RawStatement String where
    assemble (RawStatement sym args)
        = joinBy ' ' $ sym : map (wrapWith '"') args

instance Assemble RawCode String where
    assemble stmts
        = joinBy '\n' $ map assemble stmts

alias :: String -> String -> RawStatement
alias name value = RawStatement "alias" [name, value]

