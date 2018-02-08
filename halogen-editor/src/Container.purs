module Container where

import Prelude

import Audio.SoundFont (AUDIO, Instrument, loadRemoteSoundFonts)
import Audio.SoundFont.Melody.Class (class Playable, toMelody)
import Control.Monad.Aff (Aff)
import DOM.HTML.HTMLElement (offsetTop)
import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError(..), parse)
import Audio.Euterpea.Playable (PlayablePSoM(..))
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import FileInputComponent as FIC
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JS.FileIO (FILEIO, Filespec)
import MultipleSelect.Dom (SDOM)
import MultipleSelectComponent as MSC
import PlayerComponent as PC

type AppEffects eff = (au :: AUDIO, fileio :: FILEIO, sdom :: SDOM | eff)

type State =
  { a :: Maybe Boolean
  , b :: Maybe Int
  , c :: Maybe String
  , instruments :: Array Instrument
  , tuneResult :: Either PositionedParseError PSoM
  }

data Query a =
    HandleFICButton FIC.Message a
  | ReadStates a

fileInputCtx :: FIC.FileInputContext
fileInputCtx =
  { componentId : "psominput"
  , isBinary : false
  , prompt : "load a PSoM file:"
  , accept : MediaType ".psom"
  }

initialMultipleSelectState :: MSC.State
initialMultipleSelectState =
  { instruction : "choose instruments"
  , available : ("piano" : "guitar" : "mandolin" : "bouzouki" : Nil)
  , selected : Nil
  }

-- | there is no tune yet
nullTune :: Either PositionedParseError PSoM
nullTune =
  Left (PositionedParseError { pos : 0, error : "" })

parseError :: Either PositionedParseError PSoM -> String
parseError tuneResult =
  case tuneResult of
    Right _ -> "no errors"
    Left (PositionedParseError ppe) -> "parse error: " <> ppe.error


type ChildQuery = Coproduct3 FIC.Query MSC.Query PC.Query

-- slots and slot numbers
type FileInputSlot = Unit
type MultipleSelectSlot = Unit
type PlayerSlot = Unit

type ChildSlot = Either3 Unit Unit Unit

cpFICSlotNo :: CP.ChildPath FIC.Query ChildQuery FileInputSlot ChildSlot
cpFICSlotNo = CP.cp1

cpMSCSlotNo :: CP.ChildPath MSC.Query ChildQuery MultipleSelectSlot ChildSlot
cpMSCSlotNo = CP.cp2

cpPCSlotNo :: CP.ChildPath PC.Query ChildQuery PlayerSlot ChildSlot
cpPCSlotNo = CP.cp3


component ::  ∀ eff. H.Component HH.HTML Query Unit Void (Aff (AppEffects eff))
-- component ::  ∀ eff p. H.Component HH.HTML Query Unit Void (Aff (au :: AUDIO, fileio :: FILEIO, sdom :: SDOM | eff))
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { a: Nothing, b: Nothing, c: Nothing, instruments: [], tuneResult: nullTune }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
  render state = HH.div_
    [ HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "File Input Component A" ]
        , HH.slot' cpFICSlotNo unit (FIC.component fileInputCtx) unit (HE.input HandleFICButton)
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Multiple Select Component B" ]
        , HH.slot' cpMSCSlotNo unit (MSC.component initialMultipleSelectState) unit absurd
        ]
    , renderPlayer state
    , HH.p_
        [ HH.text "Last observed states:"]
    , HH.ul_
        [ HH.li_ [ HH.text ("Component A: " <> show state.a) ]
        , HH.li_ [ HH.text ("Component B: " <> show state.b) ]
        , HH.li_ [ HH.text ("tune errors: " <> (parseError state.tuneResult)) ]
        ]
    ]

  renderPlayer ::  ∀ eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (au :: AUDIO | eff))
  renderPlayer state =
    case state.tuneResult of
      Right psom ->
        HH.div
          [ HP.class_ (H.ClassName "box")]
          [ HH.slot' cpPCSlotNo unit (PC.component (PlayablePSoM psom) state.instruments) unit absurd  ]
      Left err ->
        HH.div_
          [ HH.text "no tune to play" ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (AppEffects eff))
  eval (HandleFICButton (FIC.FileLoaded filespec) next) = do
      let
        tuneResult = parse filespec.contents
      H.modify (\st -> st { tuneResult = tuneResult})
      pure next
  eval (ReadStates next) = do
    -- _ <- H.query' CP.cp1 unit (H.request FIC.GetState)
    -- _ <- H.query' CP.cp2 unit (H.request MSC.GetCount)
    -- _ <- H.query' CP.cp3 unit (H.request PC.GetValue)
    -- H.put { a, b, c }
    pure next
