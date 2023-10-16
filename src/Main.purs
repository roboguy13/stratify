module Main where

import Prelude

import Stratify.Utils

import Effect (Effect)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML.HTMLTextAreaElement as TextAreaElement
import Web.HTML.HTMLTextAreaElement (HTMLTextAreaElement)
import Web.DOM
-- import Web.DOM.HTMLInputElement (value, setValue)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.UIEvent.MouseEvent (MouseEvent, fromEvent)
import Web.Event.Event
import Web.HTML.HTMLButtonElement as Button
import Web.HTML.Event.EventTypes (click)

import Stratify.Syntax.Core.Term
import Stratify.Eval.NbE
import Stratify.Syntax.Name
import Stratify.Ppr
import Stratify.Syntax.Parser.Core
import Stratify.TypeChecker.Core

import Parsing (runParser)
import Parsing.String (eof)

import Data.Either
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = do
  win <- window
  doc <- document win
  let rootNode = toNonElementParentNode doc
  runBtn <- (Button.fromElement =<< _) <$> getElementById "runButton" rootNode
  inputArea <- (fromJust "inputArea" <<< (TextAreaElement.fromElement =<< _)) <$> getElementById "inputArea" rootNode
  outputArea <- (fromJust "outputArea" <<< (TextAreaElement.fromElement =<< _)) <$> getElementById "outputArea" rootNode
  case runBtn of
    Nothing -> error "runButton"
    Just btn -> do
      listener <- eventListener (onClick inputArea outputArea)
      addEventListener click listener false (Button.toEventTarget btn)

onClick :: HTMLTextAreaElement -> HTMLTextAreaElement -> Event -> Effect Unit
onClick inputArea outputArea event =
  case fromEvent event of
    Nothing -> pure unit
    Just _ -> do
      inputVal <- TextAreaElement.value inputArea
      case runParser inputVal (parseTerm <* eof) of
        Left e -> TextAreaElement.setValue ("Parse error: " <> show e) outputArea
        Right parsed ->
          let nameless = fromNamed parsed
              parsedNF = nf nameless
          in
          case inferType emptyNameEnv nameless of
            Left e -> TextAreaElement.setValue ("Type error: " <> e) outputArea
            Right ty -> TextAreaElement.setValue (ppr parsedNF <> "\n  : " <> ppr ty) outputArea
      -- TextAreaElement.setValue (ppr' (eval))
      -- case inputVal of
      --   ":quit" -> TextAreaElement.setValue "Goodbye!" outputArea
      --   _      -> TextAreaElement.setValue ("You typed: " <> inputVal) outputArea

fromJust :: forall a. String -> Maybe a -> a
fromJust _ (Just x) = x
fromJust msg Nothing = error $ "fromJust: " <> msg

-- ppr' (Right x) = ppr x
-- ppr' (Left y) = y