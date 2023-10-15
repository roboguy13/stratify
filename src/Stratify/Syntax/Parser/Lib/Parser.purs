module Stratify.Syntax.Parser.Lib.Parser where

import Prelude

import Stratify.Utils
import Stratify.Syntax.Parser.IxedString

import Data.Maybe
import Data.List hiding (uncons)
import Data.Tuple
import Data.Bifunctor
import Data.String
import Data.String.CodeUnits
import Data.Array as Array
import Data.String as String
import Data.Char

import Control.Lazy

import Data.Either

import Control.Alternative
import Control.Apply

newtype Parser a = Parser (IxedString -> ParserResult a)

type ParserResult a = Either ParseError (Tuple IxedString a)

type ParseError = List String

runParser :: forall a. Parser a -> String -> Either String a
runParser (Parser p) str =
    case p (mkIxedString str) of
        Left expecteds -> Left $ "Expected: " <> show expecteds
        Right (Tuple rest r) ->
            if isEmpty rest
            then Right r
            else Left $ "Incomplete parse: " <> toString rest

derive instance Functor Parser

instance Apply Parser where
    apply = ap

instance Bind Parser where
    bind (Parser k) f = Parser \x -> do
        Tuple s y <- k x
        let Parser k' = f y
        k' s

instance Applicative Parser where
    pure x = Parser (\s -> (Right (Tuple s x)))

instance Monad Parser

instance Alt Parser where
    alt (Parser p) (Parser q) = Parser \x -> p x <|> q x

instance Plus Parser where
    empty = Parser (const (Left Nil))

instance Alternative Parser

derive newtype instance Lazy (Parser a)

parseCharWhen :: String -> (Char -> Boolean) -> Parser Char
parseCharWhen msg p = Parser \str -> do
    r <- maybeToEither ("Parser reached end" : Nil) $ unconsIxed str
    if p r.head
    then Right (Tuple r.tail r.head)
    else Left (msg : Nil)

-- try :: forall a. Parser a -> Parser a
-- try = ?a

char :: Char -> Parser Char
char c = parseCharWhen (String.singleton (codePointFromChar c)) (_ == c)

oneOf :: List Char -> Parser Char
oneOf cs = parseCharWhen ("one of " <> show cs) (_ `elem` cs)

newline :: Parser Char
newline = label "newline" $ char '\n'

space :: Parser Char
space = label "space" $ oneOf (' ' : '\t' : '\n' : '\r' : Nil)

lineCommentOnly :: Parser Unit
lineCommentOnly = symbol "--" *> many (parseCharWhen "not newline" (_ /= '\n')) *> pure unit

lineComment :: Parser Unit
lineComment = lineCommentOnly *> optional newline *> pure unit

lineCommentNewline :: Parser Unit
lineCommentNewline = lineCommentOnly *> newline *> pure unit

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* many (lineCommentNewline <|> void space)

symbol :: String -> Parser String
symbol cs = map (fromCharArray <<< toUnfoldable) (go 0)
    where
        csArr = toCharArray cs

        go :: Int -> Parser (List Char)
        go i =
            case csArr Array.!! i of
                Nothing -> pure Nil
                Just c -> char c *> map (c : _) (go (i + 1))

keyword :: String -> Parser String
keyword = lexeme <<< symbol

label :: forall a. String -> Parser a -> Parser a
label name (Parser p) =
    Parser $ lmap (name : _) <<< p
