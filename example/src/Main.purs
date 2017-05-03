module Main where

import Prelude

import Control.MonadPlus (class MonadPlus)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Array as A
import Data.Either (Either(..))
import Data.Traversable as F
import Data.Maybe (Maybe(..), isNothing)
import Data.Path.Pathy ((</>), (<.>), file, currentDir, rootDir, dir)
import Data.URI as URI
import Data.URI (URIRef)

import Graphics.Canvas (CANVAS)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Leaflet as HL

import Leaflet.Core as LC
import Leaflet.Plugin.Heatmap as LH
import Leaflet.Util ((×))

data Query a
  = HandleMessage Slot HL.Message a
  | SetWidth a
  | AddMarker a
  | RemoveMarker a

type State =
  { marker ∷ Maybe LC.Marker
  }

type Input = Unit

type Slot = Int

type Effects = HA.HalogenEffects (HL.Effects (canvas ∷ CANVAS, random ∷ RANDOM))
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
      [ HH.slot 0 (HL.leaflet unit) unit (HE.input $ HandleMessage 0)
      , HH.button [ HE.onClick (HE.input_ SetWidth) ][ HH.text "resize me" ]
      , HH.button [ HE.onClick (HE.input_ AddMarker) ] [ HH.text "add marker" ]
      , HH.button [ HE.onClick (HE.input_ RemoveMarker) ] [ HH.text "remove marker" ]
      , HH.slot 1 (HL.leaflet unit) unit (HE.input $ HandleMessage 1)
      ]

  eval ∷ Query ~> DSL
  eval = case _ of
    HandleMessage 0 (HL.Initialized _) next → do
      tiles ← LC.tileLayer osmURI
      void $ H.query 0 $ H.action $ HL.AddLayers [ LC.tileToLayer tiles ]
      pure next
    HandleMessage _ (HL.Initialized leaf) next → do
      tiles ← LC.tileLayer osmURI
      heatmap ← LC.layer
      heatmapData ← liftAff mkHeatmapData
      layState ← LH.mkHeatmap LH.defaultOptions heatmapData heatmap leaf
      void $ H.query 1 $ H.action $ HL.AddLayers [ LC.tileToLayer tiles, heatmap ]
      pure next
    SetWidth next → do
      void $ H.query 0 $ H.action $ HL.SetDimension { height: Just 200, width: Just 1000 }
      pure next
    AddMarker next → do
      state ← H.get
      when (isNothing state.marker) do
        latLng ← liftAff $ LC.mkLatLng (-37.87) 175.457
        icon ← LC.icon iconConf
        marker ← LC.marker latLng >>= LC.setIcon icon
        H.modify _{ marker = Just marker }
        void $ H.query 0 $ H.action $ HL.AddLayers [ LC.markerToLayer marker ]
      pure next
    RemoveMarker next → do
      state ← H.get
      F.for_ state.marker \marker → do
        void $ H.query 0 $ H.action $ HL.RemoveLayers [ LC.markerToLayer marker ]
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

  osmURI ∷ URIRef
  osmURI =
    Left $ URI.URI
    (Just $ URI.URIScheme "http")
    (URI.HierarchicalPart
     (Just $ URI.Authority Nothing [(URI.NameAddress "{s}.tile.osm.org") × Nothing])
     (Just $ Right $ rootDir </> dir "{z}" </> dir "{x}" </> file "{y}" <.> "png"))
    Nothing
    Nothing

  mkHeatmapData
    ∷ ∀ m
    . MonadAff Effects m
    ⇒ MonadPlus m
    ⇒ MonadRec m
    ⇒ m (Array { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number })
  mkHeatmapData = do
    let
      inp = A.range 0 10000
      foldFn acc _ = do
        xDiff ← liftEff random
        lat ← LC.mkDegrees $ xDiff / 30.0 - 37.87
        yDiff ← liftEff random
        lng ← LC.mkDegrees $ yDiff / 40.0 + 175.457
        i ← map (_ / 2.0) $ liftEff random
        pure $ A.snoc acc { lat, lng, i }
    A.foldRecM foldFn [] inp

main ∷ Eff Effects Unit
main = HA.runHalogenAff $ runUI ui unit =<< HA.awaitBody
