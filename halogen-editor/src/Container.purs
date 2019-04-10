module Container where

import Prelude

import Audio.Euterpea.Playable (PlayablePSoM(..))
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Effect.Aff (Aff)
import Data.Array (cons, null)
import Data.Either (Either(..), either)
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError(..))
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Data.MediaType (MediaType(..))
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, gleitzmanNames, read)
import Data.Abc.PSoM.Polyphony (generateDSL)
import Data.Abc.Parser (parse) as ABC
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Halogen.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.SimpleButtonComponent as Button
import Halogen.PlayerComponent as PC
import Halogen.MultipleSelectComponent as MSC
import JS.FileIO (Filespec, saveTextFile)
import SampleText (frereJacques)

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either PositionedParseError PSoM
  , fileName :: Maybe String
  }

data Action =
    Init
  | HandlePSoMFile FIC.Message
  | HandleABCFile FIC.Message
  | HandleClearButton Button.Message
  | HandleSaveButton Button.Message
  | HandleSampleButton Button.Message
  | HandleNewTuneText ED.Message
  | HandleTuneIsPlaying PC.Message
  | NewInstrumentsSelection MSC.Message

psomFileInputCtx :: FIC.Context
psomFileInputCtx =
  { componentId : "psominput"
  , isBinary : false
  , prompt : "choose"
  , accept : mediaType (MediaType ".psom")
  }

abcFileInputCtx :: FIC.Context
abcFileInputCtx =
  { componentId : "abcinput"
  , isBinary : false
  , prompt : "import"
  , accept : mediaType (MediaType ".abc")
  }

multipleSelectCtx :: MSC.Context
multipleSelectCtx =
  { selectPrompt : "add an instrument:"
  , commitPrompt : "change instruments:"
  , commitButtonText : "load"
  }

initialMultipleSelectState :: ∀ i. i -> MSC.State
initialMultipleSelectState _ =
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

type ChildSlots =
  ( editor :: ED.Slot Unit
  , psomfile :: FIC.Slot Unit
  , abcfile :: FIC.Slot Unit
  , instrument :: MSC.Slot Unit
  , clear :: Button.Slot Unit
  , savefile :: Button.Slot Unit
  , sample :: Button.Slot Unit
  , player :: (PC.Slot PlayablePSoM) Unit
  )

_editor = SProxy :: SProxy "editor"
_psomfile = SProxy :: SProxy "psomfile"
_abcfile = SProxy :: SProxy "abcfile"
_instrument = SProxy :: SProxy "instrument"
_clear = SProxy :: SProxy "clear"
_savefile = SProxy :: SProxy "savefile"
_sample = SProxy :: SProxy "sample"
_player = SProxy :: SProxy "player"


component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _ =
    { instruments: []
    , tuneResult: nullTune
    , fileName: Nothing
    }

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o Aff Unit
  handleAction = case _ of
    Init -> do
      instruments <- H.liftAff $  loadRemoteSoundFonts  [AcousticGrandPiano, Vibraphone, AcousticBass]
      _ <- H.modify (\st -> st { instruments = instruments } )
      pure unit
    HandlePSoMFile (FIC.FileLoaded filespec) -> do
      _ <- H.modify (\st -> st { fileName = Just filespec.name } )
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent filespec.contents)
      _ <- H.query _player unit $ H.tell PC.StopMelody
      pure unit
    HandleABCFile (FIC.FileLoaded filespec) -> do
      state <- H.get
      let
        psomText =
          case (ABC.parse $ filespec.contents <> "\r\n") of
            Right tune ->
              let
                instrumentNames = map fst state.instruments
              in
                generateDSL tune instrumentNames
            Left err ->
              "\"" <> filespec.name <> "\"\r\n" <> "-- error in ABC: " <> (show err)
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent psomText)
      _ <- H.query _player unit $ H.tell PC.StopMelody
      pure unit
    HandleClearButton (Button.Toggled _) -> do
      _ <- H.modify (\st -> st { fileName = Nothing
                               } )
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent "")
      pure unit
    HandleSaveButton (Button.Toggled _) -> do
      maybeText <- H.query _editor unit $ H.request ED.GetText
      state <- H.get
      let
        fileName = getFileName state
        text = fromMaybe "" maybeText
        fsp = { name: fileName, contents : text} :: Filespec
      _ <- H.liftEffect $ saveTextFile fsp
      pure unit
    HandleSampleButton (Button.Toggled _) -> do
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent frereJacques)
      _ <- H.query _player unit $ H.tell PC.StopMelody
      pure unit
    HandleNewTuneText (ED.TuneResult r) -> do
      _ <- refreshPlayerState r
      _ <- H.modify (\st -> st { tuneResult = r} )
      pure unit
    NewInstrumentsSelection (MSC.CommittedSelections pendingInstrumentNames) -> do
      let
        f acc s =
          case read s of
            Just inst -> cons inst acc
            _ -> acc
        instrumentNames :: Array InstrumentName
        instrumentNames = foldl f [] pendingInstrumentNames
      instruments <- H.liftAff $ loadRemoteSoundFonts instrumentNames
      _ <- H.query _player unit $ H.tell (PC.SetInstruments instruments)
      _ <- H.modify (\st -> st { instruments = instruments})
      pure unit
    HandleTuneIsPlaying (PC.IsPlaying p) -> do
      -- we ignore this message, but if we wanted to we could
      -- disable any button that can alter the editor contents whilst the player
      -- is playing and re-enable when it stops playing
      -- _ <- H.query _editor unit $ H.tell (ED.UpdateEnabled (not p))
      -- _ <- H.query _psomfile unit $ H.tell (FIC.UpdateEnabled (not p))
      -- _ <- H.query _abcfile unit $ H.tell (FIC.UpdateEnabled (not p))
      -- _ <- H.query _clear unit $ H.tell (Button.UpdateEnabled (not p))
      -- _ <- H.query _sample unit $ H.tell (Button.UpdateEnabled (not p))
      pure unit

  render :: State -> H.ComponentHTML Action ChildSlots Aff
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
         , HH.slot _psomfile unit (FIC.component psomFileInputCtx) unit (Just <<< HandlePSoMFile)
         , HH.slot _sample unit (Button.component "example") unit (Just <<< HandleSampleButton)
         ]
        -- import
      , HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent") ]
         [  HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "import ABC:" ]
         , HH.slot _abcfile unit (FIC.component abcFileInputCtx) unit (Just <<< HandleABCFile)
         ]
      , HH.div
          -- save
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.label
             [ HP.class_ (H.ClassName "labelAlignment") ]
             [ HH.text "save or clear:" ]
          , HH.slot _savefile unit (Button.component "save") unit (Just <<< HandleSaveButton)
          -- clear
          , HH.slot _clear unit (Button.component "clear") unit (Just <<< HandleClearButton)
          ]
        -- load instruments
      , HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [
            HH.slot _instrument unit
               (MSC.component multipleSelectCtx initialMultipleSelectState) unit (Just <<< NewInstrumentsSelection)
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
            HH.slot _editor unit ED.component unit (Just <<< HandleNewTuneText)
          ]
    ]

  renderPlayer :: State -> H.ComponentHTML Action ChildSlots Aff
  renderPlayer state =
    case state.tuneResult of
      Right psom ->
        HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [
            HH.slot _player unit (PC.component (PlayablePSoM psom) state.instruments) unit (Just <<< HandleTuneIsPlaying)
          ]
      Left err ->
        HH.div_
          [  ]

  renderInstruments :: State -> H.ComponentHTML Action ChildSlots Aff
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

  renderInstrument :: Instrument -> H.ComponentHTML Action ChildSlots Aff
  renderInstrument instrument =
    HH.li
      [ HP.class_ $ ClassName "msListItemLabel" ]
      [ HH.text $ (gleitzmanName <<< fst) instrument ]
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

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState :: ∀ o.
       Either PositionedParseError PSoM
    -> H.HalogenM State Action ChildSlots o Aff Unit
refreshPlayerState tuneResult = do
  _ <- either
     (\_ -> H.query _player unit $ H.tell PC.StopMelody)
     (\psom -> H.query _player unit $ H.tell (PC.HandleNewPlayable (PlayablePSoM psom)))
     tuneResult
  pure unit
