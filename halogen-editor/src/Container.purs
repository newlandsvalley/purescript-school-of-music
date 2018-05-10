module Container where

import Prelude

import Audio.Euterpea.Playable (PlayablePSoM(..))
import Audio.SoundFont (AUDIO, Instrument, loadRemoteSoundFonts)
import Control.Monad.Aff (Aff)
import Data.Array (cons, null)
import Data.Either (Either(..), either)
import Data.Either.Nested (Either8)
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError(..))
import Data.Foldable (foldl)
import Data.Functor.Coproduct.Nested (Coproduct8)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Data.MediaType (MediaType(..))
import Data.Midi.Instrument (InstrumentName, gleitzmanName, gleitzmanNames, read)
import Data.Abc.PSoM.DSL (toDSL)
import Data.Abc.PSoM.Translation (toPSoM)
import Data.Abc.Parser (parse) as ABC
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Halogen.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.SimpleButtonComponent as Button
import Halogen.PlayerComponent as PC
import Halogen.MultipleSelectComponent as MSC
import Halogen.MultipleSelectComponent.Dom (SDOM)
import JS.FileIO (FILEIO, Filespec, saveTextFile)
import Network.HTTP.Affjax (AJAX)
import SampleText (frereJacques)


type AppEffects eff = (ajax :: AJAX, au :: AUDIO, fileio :: FILEIO, sdom :: SDOM | eff)

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either PositionedParseError PSoM
  , fileName :: Maybe String
  }

data Query a =
    HandlePSoMFile FIC.Message a
  | HandleABCFile FIC.Message a
  | HandleClearButton Button.Message a
  | HandleSaveButton Button.Message a
  | HandleSampleButton Button.Message a
  | HandleNewTuneText ED.Message a
  | HandleTuneIsPlaying PC.Message a
  | HandleMultiSelectCommit MSC.Message a

psomFileInputCtx :: FIC.Context
psomFileInputCtx =
  { componentId : "psominput"
  , isBinary : false
  , prompt : "choose"
  , accept : MediaType ".psom"
  }

abcFileInputCtx :: FIC.Context
abcFileInputCtx =
  { componentId : "abcinput"
  , isBinary : false
  , prompt : "import"
  , accept : MediaType ".abc"
  }

multipleSelectCtx :: MSC.Context
multipleSelectCtx =
  { selectPrompt : "add an instrument:"
  , commitPrompt : "change instruments:"
  , commitButtonText : "load"
  }


initialMultipleSelectState :: MSC.State
initialMultipleSelectState =
  { available : gleitzmanNames
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

-- the player is generic over a variety of playable sources of music
-- so we must specialize to Playable PSOM
type PlayerQuery = PC.Query PlayablePSoM


type ChildQuery = Coproduct8 ED.Query FIC.Query FIC.Query MSC.Query Button.Query Button.Query Button.Query PlayerQuery

-- slots and slot numbers
type FileInputSlot = Unit
type MultipleSelectSlot = Unit
type PlayerSlot = Unit
type ReplaceInstrumentsSlot = Unit
type ClearTextSlot = Unit
type SaveTextSlot = Unit
type AbcImportSlot = Unit
type SampleTextSlot = Unit
type EditorSlot = Unit

type ChildSlot = Either8 Unit Unit Unit Unit Unit Unit Unit Unit

editorSlotNo :: CP.ChildPath ED.Query ChildQuery EditorSlot ChildSlot
editorSlotNo = CP.cp1

psomFileSlotNo :: CP.ChildPath FIC.Query ChildQuery FileInputSlot ChildSlot
psomFileSlotNo = CP.cp2

abcImportSlotNo :: CP.ChildPath FIC.Query ChildQuery AbcImportSlot ChildSlot
abcImportSlotNo = CP.cp3

instrumentSelectSlotNo :: CP.ChildPath MSC.Query ChildQuery MultipleSelectSlot ChildSlot
instrumentSelectSlotNo = CP.cp4

clearTextSlotNo :: CP.ChildPath Button.Query ChildQuery ClearTextSlot ChildSlot
clearTextSlotNo = CP.cp5

saveTextSlotNo :: CP.ChildPath Button.Query ChildQuery SaveTextSlot ChildSlot
saveTextSlotNo = CP.cp6

sampleTextSlotNo :: CP.ChildPath Button.Query ChildQuery SampleTextSlot ChildSlot
sampleTextSlotNo = CP.cp7

playerSlotNo :: CP.ChildPath PlayerQuery ChildQuery PlayerSlot ChildSlot
playerSlotNo = CP.cp8


component ::  ∀ eff. Array Instrument -> H.Component HH.HTML Query Unit Void (Aff (AppEffects eff))
component initialInstruments =
  H.parentComponent
    { initialState: const (initialState initialInstruments)
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Array Instrument -> State
  initialState instruments =
    { instruments: instruments
    , tuneResult: nullTune
    , fileName: Nothing
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
  render state = HH.div_
    [ HH.h1
      [HP.class_ (H.ClassName "center") ]
      [HH.text "PureScript PSoM Editor"]
    , HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        -- load
        HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent")  ]
         [ HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "load PSoM:" ]
         , HH.slot' psomFileSlotNo unit (FIC.component psomFileInputCtx) unit (HE.input HandlePSoMFile)
         , HH.slot' sampleTextSlotNo unit (Button.component "example") unit (HE.input HandleSampleButton)
         ]
        -- import
      , HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent") ]
         [  HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "import ABC:" ]
         , HH.slot' abcImportSlotNo unit (FIC.component abcFileInputCtx) unit (HE.input HandleABCFile)
         ]
      , HH.div
          -- save
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.label
             [ HP.class_ (H.ClassName "labelAlignment") ]
             [ HH.text "save or clear:" ]
          , HH.slot' saveTextSlotNo unit (Button.component "save") unit (HE.input HandleSaveButton)
          -- clear
          , HH.slot' clearTextSlotNo unit (Button.component "clear") unit (HE.input HandleClearButton)
          ]
        -- load instruments
      , HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [
            HH.slot' instrumentSelectSlotNo unit
               (MSC.component multipleSelectCtx initialMultipleSelectState) unit (HE.input HandleMultiSelectCommit)
          ]
        -- display instruments
      , renderInstruments state
        -- player
      , renderPlayer state
      ]
      -- right pane - editor
      , HH.div
          [ HP.class_ (H.ClassName "rightPane") ]
          [
            HH.slot' editorSlotNo unit ED.component unit (HE.input HandleNewTuneText)
          ]
    ]

  renderPlayer ::  ∀ eff1. State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (au :: AUDIO | eff1))
  renderPlayer state =
    case state.tuneResult of
      Right psom ->
        HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.slot' playerSlotNo unit (PC.component (PlayablePSoM psom) state.instruments) unit (HE.input HandleTuneIsPlaying)  ]
      Left err ->
        HH.div_
          [  ]

  renderInstruments :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
  renderInstruments state =
    if (null state.instruments) then
      HH.div_ [ HH.text "wait for instruments to load"]
    else
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent") ]
        [ HH.div
           [ HP.class_ (H.ClassName "longLabel") ]
           [ HH.text "loaded instruments:" ]
        , HH.ul
          [ HP.class_ $ ClassName "msListItem" ]
          $ map renderInstrument state.instruments
        ]

  renderInstrument :: Instrument -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
  renderInstrument instrument =
    HH.li
      [ HP.class_ $ ClassName "msListItemLabel" ]
      [ HH.text $ (gleitzmanName <<< fst) instrument ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (AppEffects eff))
  eval (HandlePSoMFile (FIC.FileLoaded filespec) next) = do
    H.modify (\st -> st { fileName = Just filespec.name } )
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent filespec.contents)
    _ <- H.query' playerSlotNo unit $ H.action PC.StopMelody
    pure next
  eval (HandleABCFile (FIC.FileLoaded filespec) next) = do
    let
      psomText =
        case (ABC.parse $ filespec.contents <> "\r\n") of
          Right tune ->
            (toDSL <<< toPSoM) tune
          Left err ->
            "\"" <> filespec.name <> "\"\r\n" <> "-- error in ABC: " <> (show err)
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent psomText)
    _ <- H.query' playerSlotNo unit $ H.action PC.StopMelody
    pure next
  eval (HandleClearButton (Button.Toggled _) next) = do
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent "")
    pure next
  eval (HandleSaveButton (Button.Toggled _) next) = do
    maybeText <- H.query' editorSlotNo unit (H.request ED.GetText)
    state <- H.get
    let
      fileName = getFileName state
      text = fromMaybe "" maybeText
      fsp = { name: fileName, contents : text} :: Filespec
    _ <- H.liftEff $ saveTextFile fsp
    pure next
  eval (HandleSampleButton (Button.Toggled _) next) = do
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent frereJacques)
    _ <- H.query' playerSlotNo unit $ H.action PC.StopMelody
    pure next
  eval (HandleNewTuneText (ED.TuneResult r) next) = do
    _ <- refreshPlayerState r
    H.modify (\st -> st { tuneResult = r} )
    pure next
  eval (HandleMultiSelectCommit (MSC.CommittedSelections pendingInstrumentNames) next) = do
    let
      f acc s =
        case read s of
          Just inst -> cons inst acc
          _ -> acc
      instrumentNames :: Array InstrumentName
      instrumentNames = foldl f [] pendingInstrumentNames
    instruments <- H.liftAff $ loadRemoteSoundFonts instrumentNames
    _ <- H.query' playerSlotNo unit $ H.action (PC.SetInstruments instruments)
    H.modify (\st -> st { instruments = instruments})
    pure next
  eval (HandleTuneIsPlaying (PC.IsPlaying p) next) = do
    -- we ignore this message, but if we wanted to we could
    -- disable any button that can alter the editor contents whilst the player
    -- is playing and re-enable when it stops playing
    {-
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateEnabled (not p))
    _ <- H.query' psomFileSlotNo unit $ H.action (FIC.UpdateEnabled (not p))
    _ <- H.query' abcImportSlotNo unit $ H.action (FIC.UpdateEnabled (not p))
    _ <- H.query' clearTextSlotNo unit $ H.action (Button.UpdateEnabled (not p))
    _ <- H.query' sampleTextSlotNo unit $ H.action (Button.UpdateEnabled (not p))
    -}
    pure next


-- helpers
-- | get the file name
getFileName :: State -> String
getFileName state =
  case state.fileName of
    Just name ->
      name
    _ ->
      case state.tuneResult of
        Right { title, music } ->
          title <> ".psom"
        _ ->
          "untitled.psom"

-- refresh the state of the player by passing it the psom result
-- (if it had parsed OK)
refreshPlayerState :: ∀ eff.
       Either PositionedParseError PSoM
    -> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (AppEffects eff)) Unit
refreshPlayerState tuneResult = do
  _ <- either
    (\_ -> H.query' playerSlotNo unit $ H.action (PC.StopMelody))
    (\psom -> H.query' playerSlotNo unit $ H.action (PC.HandleNewPlayable (PlayablePSoM psom)))
    tuneResult
  pure unit
