module Leaflet.Halogen where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (class MonadAff, liftAff)

import CSS.Geometry (width, height)
import CSS.Size (px, pct)

import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable as F
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst)

import DOM (DOM)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP

import Leaflet.Core as LC
import Leaflet.Util ((∘))

import Partial.Unsafe (unsafePartial)

type State =
  { width ∷ Int
  , height ∷ Int
  , view ∷ LC.LatLng
  , zoom ∷ LC.Zoom
  , leaflet ∷ Maybe LC.Leaflet
  }

data Query a
  = Init a
  | SetDimension { width ∷ Maybe Int, height ∷ Maybe Int } a
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

type Dimensions = { width ∷ Int, height ∷ Int }

initialState ∷ Dimensions → State
initialState {width, height} =
  { height
  , width
  , view: unsafePartial fromJust $ LC.mkLatLng (-37.87) 175.457
  , zoom: unsafePartial fromJust $ LC.mkZoom 12
  , leaflet: Nothing
  }

leaflet
  ∷ ∀ e m
  . MonadAff (Effects e) m
  ⇒ H.Component HH.HTML Query (Tuple Dimensions Unit) Message m
leaflet = leaflet' $ fst >>> \dim →
  Just $ H.action $ SetDimension { width : Just dim.width, height: Just dim.height }


leaflet'
  ∷ ∀ e m i
  . MonadAff (Effects e) m
  ⇒ (Tuple Dimensions i → Maybe (Query Unit))
  → H.Component HH.HTML Query (Tuple Dimensions i) Message m
leaflet' receiver = H.lifecycleComponent
  { initialState: \(Tuple dim _) → initialState dim
  , render
  , eval
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  , receiver
  }

render ∷ State → HTML
render state =
  HH.div
    [ style do
        height $ px $ Int.toNumber state.height
        width $ px $ Int.toNumber state.width
    ]
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
  SetDimension dim next → do
    state ← H.get
    F.for_ dim.height \h →
      when (h >= 0) $ H.modify _{ height = h }
    F.for_ dim.width \w →
      when (w >= 0) $ H.modify _{ width = w }
    F.for_ state.leaflet \l → do
      liftAff $ delay $ Milliseconds 1000.0
      LC.invalidateSize true l
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
