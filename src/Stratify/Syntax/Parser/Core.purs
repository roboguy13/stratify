module Stratify.Syntax.Parser.Core
  where

import Prelude

-- import Stratify.Syntax.Parser.Lib.Parser

import Parsing as Parsing
import Parsing.String
import Parsing.Token
import Parsing.Language
import Parsing.Combinators
import Parsing.Expr
import Stratify.Syntax.Core.Term
import Stratify.Utils

import Control.Applicative
import Control.Alternative
import Data.List
import Data.Identity

import Control.Lazy

import Data.Int as Int
import Data.Maybe

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser (LanguageDef ((unGenLanguageDef haskellStyle) { reservedNames = keywords }))
lexeme = tokenParser.lexeme
keyword = tokenParser.reserved
identifier = tokenParser.identifier
symbol = tokenParser.symbol
reservedOp = tokenParser.reservedOp

-- parseTerm :: Parser SurfaceTerm
-- parseTerm = parseTerm0 (parseTerm' parseTerm0)

type Parser = Parsing.Parser String

parseTerm' :: Parser SurfaceTerm
parseTerm' =
    try parseInt
    <|> try parseBool
    <|> try (map Var parseName')
    <|> try parseIntType
    <|> try parseBoolType
    <|> (symbol "(" *> (defer \_ -> parseTerm) <* symbol ")")
    -- <|> between (string "(") (string ")") (defer \_ -> parseTerm :: Parser SurfaceTerm)

parseTerm0 :: Parser SurfaceTerm
parseTerm0 =
    try (defer \_ -> parseApp)
    <|> (defer \_ -> parseTerm')

parseTerm :: Parser SurfaceTerm
parseTerm = defer \_ ->
    try parseIf
    <|> try parseOp
    <|> try parseLam
    <|> parseTerm0

parseIntType :: Parser SurfaceTerm
parseIntType = keyword "Int" $> IntType

parseBoolType :: Parser SurfaceTerm
parseBoolType = keyword "Bool" $> BoolType

parseLam :: Parser SurfaceTerm
parseLam = do
    _ <- symbol "\\"
    x <- identifier
    _ <- symbol ":"
    ty <- parseTerm
    _ <- symbol "."
    body <- parseTerm
    pure $ Lam x ty body

parseApp :: Parser SurfaceTerm
parseApp = defer \_ -> do
    f <- parseTerm'
    args <- some parseTerm'
    pure $ foldl App f args

parseInt :: Parser SurfaceTerm
parseInt = lexeme $ (IntLit <<< go) <$> some digit
    where
        go x =
            case Int.fromString (charsToString x) of
                Nothing -> error "parseInt"
                Just r -> r

parseBool :: Parser SurfaceTerm
parseBool = lexeme $
    (keyword "True" $> BoolLit true)
    <|> (keyword "False" $> BoolLit false)

parseIf :: Parser SurfaceTerm
parseIf = lexeme do
    _ <- keyword "if"
    x <- parseTerm
    _ <- keyword "then"
    y <- parseTerm
    _ <- keyword "else"
    z <- parseTerm
    pure $ If x y z

parseOp :: Parser SurfaceTerm
parseOp = lexeme $
    buildExprParser
        [ [ binaryRight "->" fnType
          ]
        , [ binaryN "==" (theOp Equal)
          , binaryN "<" (theOp Lt)
          ]
        , [ binaryLeft "&&" (theOp And)
          , binaryLeft "||" (theOp Or)
          ]
        , [ binaryLeft "*" (theOp Mul)
          , binaryLeft "/" (theOp Div)
          , binaryLeft "+" (theOp Add)
          , binaryLeft "-" (theOp Sub)
          ]
        ]
        parseTerm0
    where
        theOp = ((Op <<< _) <<< _)

binaryN x p = Infix (reservedOp x $> p) AssocNone
binaryLeft x p = Infix (reservedOp x $> p) AssocLeft
binaryRight x p = Infix (reservedOp x $> p) AssocRight

parseName' :: Parser String
parseName' = lexeme identifier

keywords :: Array String
keywords = ["data", "where", "case", "of", "let", "in", "if", "then", "else", "not", "the", "forall", "exists", "Int", "Bool", "Type", "True", "False"]
