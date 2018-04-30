module Leaflet.Halogen where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Writer (writer)

import CSS (CSS)
import CSS.Geometry (width, height)
import CSS.Size (px, pct)
import CSS.Stylesheet (StyleM(S))

import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable as F
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))

import DOM (DOM)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP

import Leaflet.Core as LC
import Leaflet.Util ((∘))

import Partial.Unsafe (unsafePartial)

type State =
  { style ∷ CSS
  , view ∷ LC.LatLng
  , zoom ∷ LC.Zoom
  , leaflet ∷ Maybe LC.Leaflet
  }

data Query a
  = Init a
  | SetStyle CSS a
    -- Note, layers are not preserved in state, parent component must
    -- handle it.
  | AddLayers (Array LC.Layer) a
  | RemoveLayers (Array LC.Layer) a
  | GetLeaflet (Maybe LC.Leaflet → a)
  | SetView LC.LatLng a
  | SetZoom LC.Zoom a

data Message
  = Initialized LC.Leaflet

type Effects (e ∷ # Effect) =
  ( dom ∷ DOM
  | e )

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Message m

initialState ∷ Maybe CSS → State
initialState css =
  { style: maybe (S (writer (Tuple unit []))) id css
  , view: unsafePartial fromJust $ LC.mkLatLng (-37.87) 175.457
  , zoom: unsafePartial fromJust $ LC.mkZoom 12
  , leaflet: Nothing
  }

leaflet
  ∷ ∀ e m
  . MonadAff (Effects e) m
  ⇒ H.Component HH.HTML Query (Maybe CSS) Message m
leaflet = H.lifecycleComponent
  { initialState
  , render
  , eval
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  , receiver: const Nothing
  }

render ∷ State → HTML
render state =
  HH.div
    [ style state.style ]
    [ HH.div
      [ HP.ref $ H.RefLabel "leaflet"
     , style do
          height $ pct 100.0
          width $ pct 100.0
      ] [ ] ]


eval ∷ ∀ e m. MonadAff (Effects e) m ⇒ Query ~> DSL m
eval = case _ of
  Init next → do
    state ← H.get
    void $ H.getHTMLElementRef (H.RefLabel "leaflet")
      >>= F.traverse \el → do
        leaf ← LC.leaflet el
          >>= LC.setView state.view
          >>= LC.setZoom state.zoom
        H.modify _{ leaflet = Just leaf }
        H.raise $ Initialized leaf
    pure next
  SetStyle s next → do
    state ← H.get
    H.modify _{ style = s }
    pure next
  AddLayers ls next → do
    state ← H.get
    F.for_ state.leaflet \leaf →
      F.for_ ls \layer →
        void $ LC.addLayer layer leaf
    pure next
  RemoveLayers ls next → do
    state ← H.get
    F.for_ state.leaflet \leaf →
      F.for_ ls \layer →
        void $ LC.removeLayer layer leaf
    pure next
  GetLeaflet continue → do
    H.gets $ continue ∘ _.leaflet
  SetView v next → do
    state ← H.get
    F.for_ state.leaflet $ LC.setView v
    H.modify _{ view = v }
    pure next
  SetZoom zoom next → do
    state ← H.get
    F.for_ state.leaflet $ LC.setZoom zoom
    H.modify _{ zoom = zoom }
    pure next
