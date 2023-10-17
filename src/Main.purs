module Main where

import Prelude

import Stratify.Utils

import Data.Foldable

import Effect (Effect)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML.HTMLTextAreaElement as TextAreaElement
import Web.HTML.HTMLTextAreaElement (HTMLTextAreaElement)
import Web.HTML.HTMLDivElement
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLDivElement as HTMLDivElement
import Web.HTML.HTMLSpanElement (HTMLSpanElement)
import Web.HTML.HTMLSpanElement as HTMLSpanElement
import Web.HTML.HTMLElement (HTMLElement)
-- import Web.HTML.HTMLElement (createElement)
import Web.DOM
import Web.DOM.Text as Text
import Web.DOM.NodeList as NodeList
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element as Element

import Web.DOM.Node
-- import Web.DOM.HTMLInputElement (value, setValue)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.UIEvent.MouseEvent (MouseEvent, fromEvent)
import Web.UIEvent.KeyboardEvent as Keyboard
import Web.Event.Event
import Web.HTML.HTMLButtonElement as Button
import Web.HTML.Event.EventTypes (click, input)

import Stratify.Syntax.Core.Term
import Stratify.Eval.NbE
import Stratify.Syntax.Name
import Stratify.Ppr
import Stratify.Syntax.Parser.Core
import Stratify.TypeChecker.Core

import Parsing (runParser)
import Parsing.String (eof)

import Data.String (drop, length)
import Data.String as String

import Data.Either
import Data.Maybe
import Data.List

import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Ref (Ref)

main :: Effect Unit
main = do
  soFar <- Ref.new Nil
  win <- window
  doc <- document win
  let rootNode = toNonElementParentNode doc
  runBtn <- (Button.fromElement =<< _) <$> getElementById "runButton" rootNode
  terminal <- (fromJust "terminal" <<< (HTMLDivElement.fromElement =<< _)) <$> getElementById "terminal" rootNode
  inputSpan <- (fromJust "replInput" <<< (HTMLSpanElement.fromElement =<< _)) <$> getElementById "replInput" rootNode
  -- outputArea <- (fromJust "outputArea" <<< (TextAreaElement.fromElement =<< _)) <$> getElementById "outputArea" rootNode
  promptSpan <- (fromJust "promptSpan" <<< (HTMLSpanElement.fromElement =<< _)) <$> getElementById "promptSpan" rootNode
  HTMLElement.focus (HTMLSpanElement.toHTMLElement inputSpan)
  case runBtn of
    Nothing -> error "runButton"
    Just btn -> do
      listener <- eventListener (replOnEnter soFar (HTMLDocument.toDocument doc) terminal inputSpan promptSpan)
      addEventListener (EventType "keydown") listener false (HTMLSpanElement.toEventTarget inputSpan) --(Button.toEventTarget btn)
      -- listener <- eventListener (onClick inputArea outputArea)
      -- addEventListener click listener false (Button.toEventTarget btn)

prompt :: String
prompt = ">> "

replOnEnter :: Ref (List String) -> Document -> HTMLDivElement -> HTMLSpanElement -> HTMLSpanElement -> Event -> Effect Unit
replOnEnter soFar doc terminal inputSpan promptSpan event = go
  where
    go = do
      case Keyboard.fromEvent event of
        Nothing -> pure unit
        Just keyEvent -> do
          let k = Keyboard.key keyEvent
          -- log $ "replOnEnter: " <> k
          case k of
            "Enter" -> do
              inputVal <- getReplLine inputSpan
              -- log $ "inputVal = " <> inputVal
              case runParser inputVal (parseTerm <* eof) of
                Left e -> updateTerminal soFar doc terminal inputSpan promptSpan (Just inputVal) ("Parse error: " <> show e)
                Right parsed -> do
                  let nameless = fromNamed parsed
                  -- updateTerminal doc terminal ("... Nameless representation: " <> ppr nameless)
                  let parsedNF = nf nameless
                  case inferType emptyNameEnv nameless of
                    Left e -> updateTerminal soFar doc terminal inputSpan promptSpan (Just inputVal) ("Type error: " <> e)
                    Right ty -> updateTerminal soFar doc terminal inputSpan promptSpan (Just inputVal) (ppr parsedNF <> "\n  : " <> ppr ty)
              HTMLElement.focus (HTMLSpanElement.toHTMLElement inputSpan)
            _ -> pure unit

getReplLine :: HTMLSpanElement -> Effect String
getReplLine = textContent <<< HTMLSpanElement.toNode

updateTerminal :: Ref (List String) -> Document -> HTMLDivElement -> HTMLSpanElement -> HTMLSpanElement -> Maybe String -> String -> Effect Unit
updateTerminal soFar doc terminal inputSpan promptSpan promptString_maybe newString = do
  -- soFar <- textContent $ toNode terminal
  -- log $ "soFar = " <> soFar
  setTextContent "" $ toNode terminal
  let promptString = case promptString_maybe of
                       Just s -> s
                       Nothing -> "" 
  _ <- Ref.modify (_ <> ((prompt <> promptString) : newString : Nil)) soFar
  history <- Ref.read soFar
  log $ "history = " <> show history
  setLines history doc terminal

  -- Put the prompt back and reset:
  -- addBreak doc terminal
  removeChildren (HTMLSpanElement.toNode inputSpan)
  setTextContent "" (HTMLSpanElement.toNode inputSpan)
  appendChild (HTMLSpanElement.toNode promptSpan) (toNode terminal)
  appendChild (HTMLSpanElement.toNode inputSpan) (toNode terminal)
  removeChildren (HTMLSpanElement.toNode inputSpan)


setLines :: List String -> Document -> HTMLDivElement -> Effect Unit
setLines Nil _doc _div = pure unit
setLines (x : xs) doc div = do
  when (not (String.null x)) do
    xText <- createTextNode x doc
    appendChild (Text.toNode xText) (HTMLDivElement.toNode div)
    addBreak doc div
    -- case xs of
    --   Nil -> pure unit
    --   _ -> addBreak doc div
  setLines xs doc div


addBreak :: Document -> HTMLDivElement -> Effect Unit
addBreak doc div = do
  br <- createElement "br" doc
  appendChild (Element.toNode br) (toNode div)

-- readDefinitions :: HTMLTextAreaElement -> Effect String
-- readDefinitions = ?a

-- onClick :: HTMLTextAreaElement -> HTMLTextAreaElement -> Event -> Effect Unit
-- onClick inputArea outputArea event =
--   case fromEvent event of
--     Nothing -> pure unit
--     Just _ -> do
--       inputVal <- TextAreaElement.value inputArea
--       TextAreaElement.setValue "" outputArea
--       case runParser inputVal (parseTerm <* eof) of
--         Left e -> TextAreaElement.setValue ("Parse error: " <> show e) outputArea
--         Right parsed -> do
--           let nameless = fromNamed parsed
--           TextAreaElement.setValue ("... Nameless representation: " <> ppr nameless) outputArea
--           let parsedNF = nf nameless
--           case inferType emptyNameEnv nameless of
--             Left e -> TextAreaElement.setValue ("Type error: " <> e) outputArea
--             Right ty -> TextAreaElement.setValue (ppr parsedNF <> "\n  : " <> ppr ty) outputArea
--       -- TextAreaElement.setValue (ppr' (eval))
--       -- case inputVal of
--       --   ":quit" -> TextAreaElement.setValue "Goodbye!" outputArea
--       --   _      -> TextAreaElement.setValue ("You typed: " <> inputVal) outputArea

fromJust :: forall a. String -> Maybe a -> a
fromJust _ (Just x) = x
fromJust msg Nothing = error $ "fromJust: " <> msg

removeChildren :: Node -> Effect Unit
removeChildren parent = do
  children <- NodeList.toArray =<< childNodes parent
  for_ children \nodeToRemove -> removeChild nodeToRemove parent

-- -- ppr' (Right x) = ppr x
-- -- ppr' (Left y) = y