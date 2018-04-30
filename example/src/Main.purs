module Main where

import Prelude

import Control.Monad.Aff (Aff, Error, error, throwError)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Rec.Class (class MonadRec)
import CSS (CSS)
import CSS.Geometry (width, height) as CSSG
import CSS.Size (px, pct)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing, Just), isNothing, maybe)
import Data.Newtype (under)
import Data.Profunctor (lmap)
import Data.String.NonEmpty as NES
import Data.These (These(..))
import Data.Traversable as F
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component as HC
import Halogen.Component.Profunctor as HPR
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Leaflet.Core (LeafURIRef, mkLeafURIRef)
import Leaflet.Core as LC
import Leaflet.Halogen as HL
import Leaflet.Plugin.Heatmap as LH
import Leaflet.Util ((×))
import Partial.Unsafe (unsafePartial)
import URI (Authority(..), Fragment, HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), Port, Query, RelPath, RelativePart(..), RelativeRef(..), URI(..), URIRef, UserInfo) as URI
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair) as URI
import URI.HostPortPair as HostPortPair
import URI.Path.Segment (segmentNZFromString)
import URI.Path.Segment as PathSegment
import URI.Scheme.Common as Scheme
import URI.URIRef (URIRefOptions) as URI


type MainURIRef = URI.URIRef URI.UserInfo (URI.HostPortPair URI.Host URI.Port) URI.Path URI.HierPath URI.RelPath URI.Query URI.Fragment

mainURIRefOptions ∷ Record (URI.URIRefOptions URI.UserInfo (URI.HostPortPair URI.Host URI.Port) URI.Path URI.HierPath URI.RelPath URI.Query URI.Fragment)
mainURIRefOptions =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print id id
  , parsePath: pure
  , printPath: id
  , parseHierPath: pure
  , printHierPath: id
  , parseRelPath: pure
  , printRelPath: id
  , parseQuery: pure
  , printQuery: id
  , parseFragment: pure
  , printFragment: id
  }


data Query a
  = HandleMessage Slot HL.Message a
  | SetWidth a
  | AddMarker a
  | RemoveMarker a

type State =
  { marker ∷ Maybe LC.Marker
  , firstSize ∷ { width ∷ Number, height ∷ Number }
  , secondSize ∷ { width ∷ Number, height ∷ Number }
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
  , firstSize: { width: 400.0, height: 600.0 }
  , secondSize: { width: 400.0, height: 600.0 }
  }

ui ∷ H.Component HH.HTML Query Unit Void MainAff
ui = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where
  leaflet =
    HC.unComponent (\cfg →
      HC.mkComponent cfg{ receiver = \{width, height} →
        Just $ H.action $ HL.SetStyle do
          CSSG.width (px width)
          CSSG.height (px height)
    } )
    $ under HPR.ProComponent (lmap $ const Nothing) HL.leaflet

  render ∷ State → HTML
  render state =
    HH.div_
      [ HH.slot 0 leaflet state.firstSize (HE.input $ HandleMessage 0)
      , HH.button [ HE.onClick (HE.input_ SetWidth) ][ HH.text "resize me" ]
      , HH.button [ HE.onClick (HE.input_ AddMarker) ] [ HH.text "add marker" ]
      , HH.button [ HE.onClick (HE.input_ RemoveMarker) ] [ HH.text "remove marker" ]
      , HH.slot 1 leaflet state.secondSize (HE.input $ HandleMessage 1)
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
      H.modify _{ firstSize = { height: 200.0, width: 1000.0 } }
      pure next
    AddMarker next → do
      state ← H.get
      when (isNothing state.marker) do
        latLng ← liftAff $ throwMaybe $ LC.mkLatLng (-37.87) 175.457
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

  iconConf ∷ { iconUrl ∷ LeafURIRef, iconSize ∷ LC.Point }
  iconConf =
    { iconUrl: mkLeafURIRef
      { uri: Right $ URI.RelativeRef
        (URI.RelativePartNoAuth $ Just $ Left $ URI.PathAbsolute $ Just $ Tuple (segmentNZFromString $ unsafePartial $ NES.unsafeFromString "marker.svg") [])
        Nothing
        Nothing
      , opts: mainURIRefOptions
      }
    , iconSize: 40 × 40
    }

  osmURI ∷ LeafURIRef
  osmURI = mkLeafURIRef
    { uri: Left $ URI.URI
      Scheme.http
      (URI.HierarchicalPartAuth
        (URI.Authority Nothing (Just $ This $ URI.NameAddress $ RegName.fromString $ unsafePartial $ NES.unsafeFromString "{s}.tile.osm.org"))
        (URI.Path $ map PathSegment.segmentFromString ["{z}", "{x}", "{y}.png"]))
      Nothing
      Nothing
    , opts: mainURIRefOptions
    }

  mkHeatmapData
    ∷ ∀ m
    . MonadAff Effects m
    ⇒ MonadRec m
    ⇒ MonadError Error m
    ⇒ m (Array { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number })
  mkHeatmapData = do
    let
      inp = A.range 0 10000
      foldFn acc _ = do
        xDiff ← liftEff random
        lat ← throwMaybe (LC.mkDegrees $ xDiff / 30.0 - 37.87)
        yDiff ← liftEff random
        lng ← throwMaybe (LC.mkDegrees $ yDiff / 40.0 + 175.457)
        i ← map (_ / 2.0) $ liftEff random
        pure $ A.snoc acc { lat, lng, i }
    A.foldRecM foldFn [] inp

throwMaybe ∷ ∀ a m. MonadError Error m ⇒ Maybe a → m a
throwMaybe = maybe (throwError (error "throwMaybe")) pure

main ∷ Eff Effects Unit
main = HA.runHalogenAff $ runUI ui unit =<< HA.awaitBody
