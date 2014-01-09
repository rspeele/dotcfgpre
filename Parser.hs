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

identifier :: Parser ByteString
identifier =
    bmany1 $ satisfy (`S.member` idc)
    where idc = S.fromList $ concat
                [['a'..'z']
                ,['A'..'Z']
                ,['0'..'9']
                ,"_"
                ]

requiredSpace :: Parser ()
requiredSpace = many1 (satisfy (`elem` " \t")) *> return ()

quotedArgument :: Parser ByteString
quotedArgument = char '"' *> bmanyTill anyChar (char '"')

argument :: Parser ByteString
argument = quotedArgument <|> identifier

rawStatement :: Parser RawStatement
rawStatement =
    RawStatement <$> identifier <*> many (try $ requiredSpace >> argument)

block :: Parser [Statement]
block = char '[' *> statements <* char ']'

terminator :: Parser ()
terminator = oneOf ";\r\n" *> return () <|> eof
terminators :: Parser ()

terminators = terminator <* ignoredSpace

ignoredSpace :: Parser ()
ignoredSpace = many (terminator <|> requiredSpace) *> return ()

statement :: Parser Statement
statement =
    try (Raw <$> rawStatement)
    <|> (Block <$> block)

wrap x = between x x

statements :: Parser [Statement]
statements = ignoredSpace *> many (statement <* terminators) <* ignoredSpace

test s = parse statement "" (B.pack s)