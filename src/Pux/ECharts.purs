module Pux.ECharts where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Aff.AVar (AVAR)

import CSS.Geometry (width, height)
import CSS.Size (px)

import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts.Chart as EC
import ECharts.Theme as ETheme
import ECharts.Types as ET
import ECharts.Monad as EM
import ECharts.Types.Phantom as ETP

import Pux (EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.ECharts.Utils (getElementById)

import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (id)
import Text.Smolder.Markup ((!))

import Prelude (($), (<<<), (*>), bind, discard)

type Effects eff =
  ( echarts ∷ ET.ECHARTS
  , dom ∷ DOM
  , avar ∷ AVAR
  , exception ∷ EXCEPTION
  , ref ∷ REF
  | eff
  )

type State =
  { chartId :: ElementId
  , chart ∷ Maybe ET.Chart
  , width ∷ Int
  , height ∷ Int
  , options :: EM.DSL ETP.OptionI
  }

data Event
  = -- | Initialise the chart (perhaps with a theme)
    Render (Maybe ETheme.Theme)
    -- | Fires when the chart has initialised
  | Initialised ET.Chart
  | Dispose
    -- | Re-render the chart with updated options
  | Refresh
  | Resize
  | Clear
    -- | Only a teensy-weensy hack. We want to trigger events on the next
    --   'tick', because we want them to happen after the view function
    --   completes. So `Defer` just re-raises the wrapped event.
  | Defer Event

-- | Creates a container which will subsequently be modified effectfully.
view
  ∷ ∀e. State → HTML e
view state =
  div
    ! id (unwrap state.chartId)
    ! style do
        height $ px $ toNumber state.height
        width $ px $ toNumber state.width
    $ mempty

foldp
  :: ∀ fx
   . Event
  -> State
  -> EffModel State Event (Effects fx)
foldp (Render theme) state =
  { state : state
  , effects :
    [
      liftEff (getElementById state.chartId) >>= traverse \el -> do
        chart <- liftEff $ maybe EC.init EC.initWithTheme theme el
        EC.setOption state.options chart
        pure $ Initialised chart
    ]
  }
foldp (Initialised chart) state = noEffects $ state { chart = Just chart }
foldp Dispose state =
  { state : state
  , effects : [
      (for_ state.chart $ liftEff <<< EC.dispose) *> pure Nothing
    ]
  }
foldp Refresh state =
  { state : state
  , effects :  [
      (for_ state.chart $ liftEff <<< EC.setOption state.options)
      *> pure Nothing
    ]
  }
foldp Resize state =
  { state : state
  , effects :  [
      (for_ state.chart $ liftEff <<< EC.resize) *> pure Nothing
    ]
  }
foldp Clear state =
  { state : state
  , effects :  [
      (for_ state.chart $ liftEff <<< EC.clear) *> pure Nothing
    ]
  }
foldp (Defer evt) state =
  { state : state
  , effects: [ pure $ Just evt ]
  }
