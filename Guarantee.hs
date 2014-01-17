{-# LANGUAGE TemplateHaskell #-}
module Guarantee (guarantee, parses, mustParse, mustNotParse) where
import Control.Monad (unless)
import Language.Haskell.TH (reportError)
import Language.Haskell.TH.Syntax
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Text.Parsec.ByteString
import Text.Parsec hiding ((<|>), many)
import Control.Applicative

testParse
    :: (Eq a, Show a) =>
       Q ()
    -> Q ()
    -> (a -> a -> Q ())
    -> Parser a
    -> ByteString
    -> a
    -> Q [Dec]
testParse success failure mismatch p s r = do
  case parse (p <* eof) "" s of
    Right a ->
        if a == r
        then success
        else mismatch a r
    Left _ -> failure
  return []

mustParse :: (Eq a, Show a) => Parser a -> ByteString -> a -> Q [Dec]
mustParse = testParse (return ()) failed mismatch
    where
      failed = reportError "Guarantee broken: parse failed"
      mismatch a r = reportError $ concat ["Parse did not match: ", show a, " != ", show r]

mustNotParse :: (Eq a, Show a) => Parser a -> ByteString -> a -> Q [Dec]
mustNotParse = testParse broken (return ()) extraBroken
    where
      broken = reportError "Guarantee broken: parse succeeeded"
      extraBroken a r = reportError $ concat [msg, show a, " != ", show r]
          where msg = "Guarantee broken: parse succeeded, but didn't match: "

parses p s r =
    case parse (p <* eof) "" $ B.pack s of
      Right a -> a == r
      Left _ -> False

guarantee test = do
  unless test $ reportError $ "Guarantee broken"
  return []
