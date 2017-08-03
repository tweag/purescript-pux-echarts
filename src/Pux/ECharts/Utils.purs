module Pux.ECharts.Utils
  (  getElementById ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), fromJust)
import Data.Either (either)
import Data.Foreign (toForeign)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlDocumentToNonElementParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode as NEPN
import DOM.Node.Types (ElementId)

import Partial.Unsafe (unsafePartial)

getElementById
  ∷ ∀ eff
  . ElementId
  → Eff (dom ∷ DOM | eff) (Maybe HTMLElement)
getElementById elementId = do
  win ← window
  doc ← document win
  el ← NEPN.getElementById elementId (htmlDocumentToNonElementParentNode doc)
  pure $ either (const Nothing) Just $ runExcept $ readHTMLElement (toForeign <<< unsafePartial fromJust $ el)
