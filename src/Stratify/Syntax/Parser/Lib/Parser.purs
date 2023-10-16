module Stratify.Syntax.Parser.Lib.Parser
  ( Batch
  , Operator(..)
  , ParseError
  , Parser(..)
  , ParserResult
  , addPrecLevel
  , addToBatch
  , alpha
  , binaryLeft
  , binaryN
  , binaryRight
  , char
  , choice
  , digit
  , hidden
  , inParens
  , initialBatch
  , keyword
  , label
  , lexeme
  , lexemeMany
  , lineComment
  , lineCommentOnly
  , newline
  , oneOf
  , option
  , pInfixL
  , pInfixN
  , pInfixR
  , pTerm
  , parseBaseName
  , parseCharWhen
  , parseName
  , precedenceTable
  , prefix
  , runParser
  , space
  , string
  , symbol
  , unParser
  )
  where

import Prelude

import Stratify.Utils
import Stratify.Syntax.Parser.IxedString

import Data.Maybe
import Data.List hiding (uncons)
import Data.Tuple
import Data.Bifunctor
import Data.String
import Data.String.CodeUnits
import Data.CodePoint.Unicode
import Data.Array as Array
import Data.String as String
import Data.Char
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmpty

import Control.Lazy
import Control.Comonad

import Data.Either

import Control.Alternative
import Control.Apply

import Data.Foldable as Foldable
import Data.Foldable (class Foldable)

newtype Parser a = Parser (IxedString -> ParserResult a)

unParser :: forall a. Parser a -> (IxedString -> ParserResult a)
unParser (Parser p) = p


-- TODO: Add support for better error messages
type ParserResult a = List (Tuple IxedString a)

type ParseError = List String

runParser :: forall a. Parser a -> String -> Either String a
runParser (Parser p) str =
    case p (mkIxedString str) of
        -- Left expecteds -> Left $ "Expected: " <> show expecteds
        Nil -> Left "Parse error"
        Tuple rest r : _ ->
            if isEmpty rest
            then Right r
            else Left $ "Incomplete parse: " <> toString rest

derive instance Functor Parser

instance Apply Parser where
    apply = ap

instance Bind Parser where
    bind (Parser k) f = Parser \x -> do
        Tuple s y <- k x
        unParser (f y) s

        -- xs <- k x
        -- let gs = map (unParser <<< f <<< snd) xs
        --     ys = fst xs
        -- extract $ gs <*> ys -- TODO: Is this right?
        -- Tuple s y <- k x
        -- let Parser k' = f y
        -- k' s

instance Applicative Parser where
    pure x = Parser (\s -> List.singleton (Tuple s x))

instance Monad Parser

instance Alt Parser where
    alt (Parser p) (Parser q) = Parser \x -> p x <|> q x

instance Plus Parser where
    empty = Parser (const Nil)

instance Alternative Parser

derive newtype instance Lazy (Parser a)

parseCharWhen :: String -> (Char -> Boolean) -> Parser Char
parseCharWhen msg p = Parser \str -> do
    case unconsIxed str of
        Nothing -> Nil --Left ("Parser reached end" : Nil)
        Just r ->
            if p r.head
            then List.singleton (Tuple r.tail r.head)
            else Nil

-- try :: forall a. Parser a -> Parser a
-- try (Parser p) = Parser \x ->
--     case p x of
--         Left _ -> ?a
--         Right r -> pure r

char :: Char -> Parser Char
char c = parseCharWhen (String.singleton (codePointFromChar c)) (_ == c)

oneOf :: List Char -> Parser Char
oneOf cs = parseCharWhen ("one of " <> show cs) (_ `elem` cs)

newline :: Parser Char
newline = label "newline" $ char '\n'

space :: Parser Char
space = label "space" $ oneOf (' ' : '\t' : '\n' : '\r' : Nil)

lineCommentOnly :: Parser Unit
lineCommentOnly = string "--" *> many (parseCharWhen "not newline" (_ /= '\n')) *> pure unit

lineComment :: Parser Unit
lineComment = lineCommentOnly *> optional newline *> pure unit

lineCommentNewline :: Parser Unit
lineCommentNewline = lineCommentOnly *> newline *> pure unit

lexemeMany :: forall a. Parser a -> Parser a
lexemeMany p = p <* many (lineCommentNewline <|> void space)

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* some (lineCommentNewline <|> void space)
    -- where
    --     consumeSpace = Parser \x ->
    --         case unconsIxed str of
    --             Nothing -> Tuple x unit : Nil
    --             Just r ->



inParens :: forall a. Parser a -> Parser a
inParens p = symbol "(" *> p <* symbol ")"

string :: String -> Parser String
string cs = map charsToString (go 0)
    where
        csArr = toCharArray cs

        go :: Int -> Parser (List Char)
        go i =
            case csArr Array.!! i of
                Nothing -> pure Nil
                Just c -> char c *> map (c : _) (go (i + 1))

symbol :: String -> Parser String
symbol = lexemeMany <<< string

keyword :: String -> Parser String
keyword = lexeme <<< string

parseName :: forall f. Foldable f => f String -> Parser String
parseName keywords = label "identifier" $ lexeme $ do
    n <- parseBaseName
    guard (n `notElem` keywords)
    pure n

parseBaseName :: Parser String
parseBaseName = charsToString <$> ((:) <$> nameInitial <*> many nameRest)
    where
        nameInitial = alpha <|> char '_'
        nameRest = nameInitial <|> digit <|> char '\''

alpha :: Parser Char
alpha = parseCharWhen "alpha" (isAlpha <<< codePointFromChar)

digit :: Parser Char
digit = parseCharWhen "digit" (isDecDigit <<< codePointFromChar)

label :: forall a. String -> Parser a -> Parser a
label _ p = p -- TODO: Improve
-- label name (Parser p) =
--     -- Parser $ lmap (name : _) <<< p

option :: forall a. a -> Parser a -> Parser a
option x p = p <|> pure x

choice :: forall f a. Foldable f => f (Parser a) -> Parser a
choice = Foldable.oneOf

-- TODO: Is this right?
hidden :: forall a. Parser a -> Parser a
hidden p = p
-- hidden (Parser p) = Parser \x ->
--     case p x of
--         Left Nil -> Left Nil
--         Left (_ : xs) -> Left xs
--         Right r -> Right r

-- Based on Text.Megaparsec.Expr from megaparsec on Hackage
data Operator a
    = InfixL (Parser (a -> a -> a))
    | InfixR (Parser (a -> a -> a))
    | InfixN (Parser (a -> a -> a))
    | Prefix (Parser (a -> a))

type Batch a =
    { infixls :: List (Parser (a -> a -> a))
    , infixrs :: List (Parser (a -> a -> a))
    , infixns :: List (Parser (a -> a -> a))
    , prefixes :: List (Parser (a -> a))
    }

binaryLeft :: forall a. String -> (a -> a -> a) -> Operator a
binaryLeft name f = InfixL (f <$ symbol name)

binaryRight :: forall a. String -> (a -> a -> a) -> Operator a
binaryRight name f = InfixR (f <$ symbol name)

binaryN :: forall a. String -> (a -> a -> a) -> Operator a
binaryN name f = InfixN (f <$ symbol name)

prefix :: forall a. String -> (a -> a) -> Operator a
prefix name f = Prefix (f <$ symbol name)

precedenceTable :: forall a. Parser a -> Array (Array (Operator a)) -> Parser a
precedenceTable = foldl addPrecLevel

addPrecLevel :: forall a. Parser a -> Array (Operator a) -> Parser a
addPrecLevel term ops = do
    x <- term'
    label "operator" $ choice [rightAssocs x, leftAssocs x, nonAssocs x, pure x]
    where
        batch = foldr addToBatch initialBatch ops
        term' = pTerm (choice batch.prefixes) term empty
        rightAssocs = pInfixL (choice batch.infixrs) term'
        leftAssocs = pInfixR (choice batch.infixls) term'
        nonAssocs = pInfixN (choice batch.infixns) term'

pTerm :: forall a. Parser (a -> a) -> Parser a -> Parser (a -> a) -> Parser a
pTerm prefix term postfix = do
    pre <- option identity (hidden prefix)
    x <- term
    post <- option identity (hidden postfix)
    pure $ post $ pre x

pInfixL :: forall a. Parser (a -> a -> a) -> Parser a -> a -> Parser a
pInfixL op p x = do
    f <- op
    y <- p
    let r = f x y
    pInfixL op p r <|> pure r

pInfixR :: forall a. Parser (a -> a -> a) -> Parser a -> a -> Parser a
pInfixR op p x = do
    f <- op
    y <- p >>= \r -> pInfixR op p r <|> pure r
    pure $ f x y

pInfixN :: forall a. Parser (a -> a -> a) -> Parser a -> a -> Parser a
pInfixN op p x = do
    f <- op
    y <- p
    pure $ f x y

initialBatch :: forall a. Batch a
initialBatch = { infixls: Nil, infixrs: Nil, infixns: Nil, prefixes: Nil }

addToBatch :: forall a. Operator a -> Batch a -> Batch a
addToBatch (InfixL x) batch = batch { infixls = x : batch.infixls }
addToBatch (InfixR x) batch = batch { infixrs = x : batch.infixrs }
addToBatch (InfixN x) batch = batch { infixns = x : batch.infixns }
addToBatch (Prefix x) batch = batch { prefixes = x : batch.prefixes }
