{-# LANGUAGE OverloadedStrings #-}
module Parser where
import Language
import RawCfg
import Control.Applicative
import Data.Char (isSpace)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Text.Parsec hiding ((<|>), many)

bpack m f = m f >>= return . B.pack
bmany :: Parser Char -> Parser ByteString
bmany = bpack many
bmany1 = bpack many1
bmanyTill p end = manyTill p end >>= return . B.pack
bstring s = string s >>= return . B.pack

anyOf :: [Parser a] -> Parser a
anyOf = foldr1 (<|>) . map try

identifier :: Parser ByteString
identifier =
    bmany1 $ satisfy (`S.member` idc)
    where idc = S.fromList $ concat
                [['a'..'z']
                ,['A'..'Z']
                ,['0'..'9']
                ,"_+-"
                ]

terminator :: Parser ()
terminator = oneOf ";\r\n" *> return ()
             <?> "terminator"

terminators :: Parser ()
terminators = many requiredSpace *> terminator <* ignoredSpace
              <?> "terminators"

ignoredSpace :: Parser ()
ignoredSpace = eof <|> many (terminator <|> requiredSpace) *> return ()
               <?> "whitespace"

requiredSpace :: Parser ()
requiredSpace = many1 (satisfy (`elem` " \t")) *> return ()

quotedArgument :: Parser ByteString
quotedArgument = char '"' *> bmanyTill anyChar (char '"')

argument :: Parser ByteString
argument = quotedArgument <|> identifier

raw :: Parser SmartStmt
raw = DirectStmt <$> raw'
    where
      raw' = RawStmt <$> identifier <*> args
      args = many $ try $ requiredSpace *> argument

block :: Parser SmartStmt
block = BlockStmt <$> (char '[' *> statements <* char ']')

alias :: Parser SmartStmt
alias = AliasStmt <$> n <*> st
    where
      n = bstring "alias" *> requiredSpace *> identifier
      st = requiredSpace *> smartSt

bind :: Parser SmartStmt
bind = BindStmt <$> k <*> st
    where
      k = bstring "bind" *> requiredSpace *> identifier
      st = requiredSpace *> smartSt

condition :: Parser Condition
condition = buildExpressionParser table (term <* ignoredSpace)
    where
      term = parens <|> check
      parens = char '(' *> ignoredSpace *> condition <* char ')'
      check =
          anyOf
          [ Check <$> (char '+' *> identifier)
          , Not . Check <$> (char '-' *> identifier) ]
      table = [ [ binary "and" And ]
              , [ binary "or" Or ] ]
      binary name fun = Infix (bstring name <* ignoredSpace *> return fun) AssocLeft

smartSt :: Parser SmartStmt
smartSt =
    anyOf
    [ ifSt
    , block
    , alias
    , bind
    , raw
    ]

ifSt :: Parser SmartStmt
ifSt = IfStmt <$> ifPart <*> thenPart <*> elsePart
    where
      ifPart = bstring "if" *> requiredSpace *> condition
      thenPart = bstring "then" *> requiredSpace *> smartSt
      elsePart = requiredSpace *> bstring "else" *> requiredSpace *> smartSt
                 <|> (return $ BlockStmt [])

statements :: Parser [SmartStmt]
statements = ignoredSpace *> body <* ignoredSpace
    where
      body = do
        first <- optionMaybe smartSt
        case first of
          Just stmt ->
              do
                rest <- many $ try $ terminators *> smartSt
                return $ stmt : rest
          Nothing -> return []

parseStmts :: ByteString -> Either ParseError [SmartStmt]
parseStmts s = parse statements "" s