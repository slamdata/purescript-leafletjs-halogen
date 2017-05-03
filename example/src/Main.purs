module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)

import Data.Either (Either(..))
import Data.Foldable as F
import Data.Maybe (Maybe(..), isNothing)
import Data.Path.Pathy ((</>), (<.>), file, currentDir, rootDir, dir)
import Data.URI as URI
import Data.URI (URIRef)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Leaflet as HL

import Leaflet.Core as LC
import Leaflet.Util ((×))

import Debug.Trace as DT

data Query a
  = HandleMessage HL.Message a
  | SetWidth a
  | AddMarker a
  | RemoveMarker a

type State =
  { marker ∷ Maybe LC.Marker
  }

type Input = Unit
type Slot = Unit
type Effects = HA.HalogenEffects (HL.Effects ())
type MainAff = Aff Effects
type HTML = H.ParentHTML Query HL.Query Slot MainAff
type DSL = H.ParentDSL State Query HL.Query Slot Void MainAff

initialState ∷ Input → State
initialState _ =
  { marker: Nothing
  }

ui ∷ H.Component HH.HTML Query Unit Void MainAff
ui = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where
  render ∷ State → HTML
  render state =
    HH.div_
      [ HH.slot unit (HL.leaflet unit) unit (HE.input HandleMessage)
      , HH.button [ HE.onClick (HE.input_ SetWidth) ][ HH.text "resize me" ]
      , HH.button [ HE.onClick (HE.input_ AddMarker) ] [ HH.text "add marker" ]
      , HH.button [ HE.onClick (HE.input_ RemoveMarker) ] [ HH.text "remove marker" ]
      ]

  eval ∷ Query ~> DSL
  eval = case _ of
    HandleMessage HL.Initialized next → do
      DT.traceAnyA "initialized"
      let
        osmURI =
          Left $ URI.URI
            (Just $ URI.URIScheme "http")
            (URI.HierarchicalPart
             (Just $ URI.Authority Nothing [(URI.NameAddress "{s}.tile.osm.org") × Nothing])
             (Just $ Right $ rootDir </> dir "{z}" </> dir "{x}" </> file "{y}" <.> "png"))
            Nothing
            Nothing
      tiles ← LC.tileLayer osmURI
      void $ H.query unit $ H.action $ HL.AddLayers [ LC.tileToLayer tiles ]
      pure next
    SetWidth next → do
      void $ H.query unit $ H.action $ HL.SetDimension { height: Just 200, width: Just 1000 }
      pure next
    AddMarker next → do
      state ← H.get
      when (isNothing state.marker) do
        latLng ← liftAff $ LC.mkLatLng (-37.87) 175.457
        icon ← LC.icon iconConf
        marker ← LC.marker latLng >>= LC.setIcon icon
        H.modify _{ marker = Just marker }
        void $ H.query unit $ H.action $ HL.AddLayers [ LC.markerToLayer marker ]
      pure next
    RemoveMarker next → do
      state ← H.get
      F.for_ state.marker \marker → do
        void $ H.query unit $ H.action $ HL.RemoveLayers [ LC.markerToLayer marker ]
        H.modify _{ marker = Nothing }
      pure next

  iconConf ∷ { iconUrl ∷ URIRef, iconSize ∷ LC.Point }
  iconConf =
    { iconUrl: Right $ URI.RelativeRef
        (URI.RelativePart Nothing $ Just $ Right $ currentDir </> file "marker" <.> "svg")
        Nothing
        Nothing
    , iconSize: 40 × 40
    }

main ∷ Eff _ Unit
main = HA.runHalogenAff $ runUI ui unit =<< HA.awaitBody
