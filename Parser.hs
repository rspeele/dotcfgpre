module Parser where
import LowLevel
import HighLevel
import Control.Applicative
import Data.Char (isSpace)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec.ByteString
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

raw :: Parser Statement
raw = Raw <$> raw'
    where
      raw' = RawStatement <$> identifier <*> args
      args = many $ try $ requiredSpace *> argument

block :: Parser Statement
block = Block <$> (char '[' *> statements <* char ']')

alias :: Parser Statement
alias = Alias <$> n <*> st
    where
      n = bstring "alias" *> requiredSpace *> identifier
      st = requiredSpace *> statement

bind :: Parser Statement
bind = Bind <$> k <*> st
    where
      k = bstring "bind" *> requiredSpace *> identifier
      st = requiredSpace *> statement

ifStmt :: Parser Statement
ifStmt = If <$> ifPart <*> (requiredSpace *> statement) <*> optionMaybe elsePart
    where
      ifPart = bstring "if" *> requiredSpace *> condPart
      condPart =
          anyOf
          [ pressed <$> (bstring "+" *> identifier)
          , released <$> (bstring "-" *> identifier)
          ]
      elsePart = requiredSpace *> bstring "else" *> requiredSpace *> statement

statement :: Parser Statement
statement =
    anyOf
    [ block
    , alias
    , bind
    , ifStmt
    , raw
    ]

statements :: Parser [Statement]
statements = ignoredSpace *> body <* ignoredSpace
    where
      body = do
        first <- optionMaybe statement
        case first of
          Just stmt ->
              do
                rest <- many $ try $ terminators *> statement
                return $ stmt : rest
          Nothing -> return []

parseStatements :: ByteString -> Either ParseError [Statement]
parseStatements s = parse statements "" s