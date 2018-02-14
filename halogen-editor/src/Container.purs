module Container where

import Prelude

import Audio.Euterpea.Playable (PlayablePSoM(..))
import Audio.SoundFont (AUDIO, Instrument, loadRemoteSoundFonts)
import Control.Monad.Aff (Aff)
import Data.Array (cons, null)
import Data.Either (Either(..))
import Data.Either.Nested (Either7)
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError(..))
import Data.Foldable (foldl)
import Data.Functor.Coproduct.Nested (Coproduct7)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Data.MediaType (MediaType(..))
import Data.Midi.Instrument (InstrumentName, gleitzmanName, gleitzmanNames, read)
import Data.Abc.PSoM.DSL (toDSL)
import Data.Abc.PSoM.Translation (toPSoM)
import Data.Abc.Parser (parse) as ABC
import EditorComponent as ED
import FileInputComponent as FIC
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JS.FileIO (FILEIO, Filespec, saveTextFile)
import MultipleSelect.Dom (SDOM)
import MultipleSelectComponent as MSC
import Network.HTTP.Affjax (AJAX)
import PlayerComponent as PC
import SimpleButtonComponent as Button

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
  | HandleNewTuneText ED.Message a
  | HandleMultiSelectCommit MSC.Message a

psomFileInputCtx :: FIC.FileInputContext
psomFileInputCtx =
  { componentId : "psominput"
  , isBinary : false
  , prompt : "load a PSoM file:"
  , accept : MediaType ".psom"
  }

abcFileInputCtx :: FIC.FileInputContext
abcFileInputCtx =
    { componentId : "abcinput"
    , isBinary : false
    , prompt : "import an ABC file:"
    , accept : MediaType ".abc"
    }

initialMultipleSelectState :: MSC.State
initialMultipleSelectState =
  { instruction : "add an instrument"
  , available : gleitzmanNames
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

type ChildQuery = Coproduct7 ED.Query FIC.Query FIC.Query MSC.Query Button.Query Button.Query PC.Query

-- slots and slot numbers
type FileInputSlot = Unit
type MultipleSelectSlot = Unit
type PlayerSlot = Unit
type ReplaceInstrumentsSlot = Unit
type ClearTextSlot = Unit
type SaveTextSlot = Unit
type AbcImportSlot = Unit
type EditorSlot = Unit

type ChildSlot = Either7 Unit Unit Unit Unit Unit Unit Unit

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

playerSlotNo :: CP.ChildPath PC.Query ChildQuery PlayerSlot ChildSlot
playerSlotNo = CP.cp7


component ::  ∀ eff. Array Instrument -> H.Component HH.HTML Query Unit Void (Aff (AppEffects eff))
-- component ::  ∀ eff p. H.Component HH.HTML Query Unit Void (Aff (au :: AUDIO, fileio :: FILEIO, sdom :: SDOM | eff))
component instruments =
  H.parentComponent
    { initialState: const (initialState instruments)
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
    [ HH.div_
      -- left pane
      [
        -- load
        HH.div
         [ HP.class_ (H.ClassName "box")]
         [ HH.h1_ [ HH.text "File Input Component 1" ]
         , HH.slot' psomFileSlotNo unit (FIC.component psomFileInputCtx) unit (HE.input HandlePSoMFile)
         ]
        -- import
      , HH.div
         [ HP.class_ (H.ClassName "box")]
         [ HH.h1_ [ HH.text "File Input Component 2" ]
         , HH.slot' abcImportSlotNo unit (FIC.component abcFileInputCtx) unit (HE.input HandleABCFile)
         ]
        -- save
      , HH.div
          [ HP.class_ (H.ClassName "box")]
          [ HH.h1_ [ HH.text "Save Text Component" ]
          , HH.slot' saveTextSlotNo unit (Button.component "save") unit (HE.input HandleSaveButton)
          ]
        -- clear
      , HH.div
          [ HP.class_ (H.ClassName "box")]
          [ HH.h1_ [ HH.text "Clear Text Component" ]
          , HH.slot' clearTextSlotNo unit (Button.component "clear") unit (HE.input HandleClearButton)
          ]
        -- display instruments
      , renderInstruments state
        -- load instruments
      , HH.div
          [ HP.class_ (H.ClassName "box")]
          [ HH.h1_ [ HH.text "Multiple Select Component" ]
          , HH.slot' instrumentSelectSlotNo unit (MSC.component initialMultipleSelectState) unit (HE.input HandleMultiSelectCommit)
          ]
        -- player
      , renderPlayer state
      ]
      -- right pane
      , HH.div
          [ HP.class_ (H.ClassName "box")]
          [ HH.h1_ [ HH.text "Text Area Component" ]
          , HH.slot' editorSlotNo unit (ED.component "foo") unit (HE.input HandleNewTuneText)
          ]
    ]

  renderPlayer ::  ∀ eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (au :: AUDIO | eff))
  renderPlayer state =
    case state.tuneResult of
      Right psom ->
        HH.div
          [ HP.class_ (H.ClassName "box")]
          [ HH.slot' playerSlotNo unit (PC.component (PlayablePSoM psom) state.instruments) unit absurd  ]
      Left err ->
        HH.div_
          [ HH.text "no tune to play" ]

  renderInstruments :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
  renderInstruments state =
    if (null state.instruments) then
      HH.div_ [ HH.text "wait for instruments to load"]
    else
      HH.div_
        [ HH.text "loaded instruments"
        , HH.ul_ $ map renderInstrument state.instruments
        ]

  renderInstrument :: Instrument -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
  renderInstrument instrument =
    HH.li_
      [ HH.text $ (gleitzmanName <<< fst) instrument ]


  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (AppEffects eff))
  eval (HandlePSoMFile (FIC.FileLoaded filespec) next) = do
    _ <- H.query' editorSlotNo unit $ H.action (ED.UpdateContent filespec.contents)
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
  eval (HandleNewTuneText (ED.TuneResult r) next) = do
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
    H.modify (\st -> st { instruments = instruments})
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