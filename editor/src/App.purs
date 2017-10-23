module App where

import Audio.Euterpea.Player as Player
import Text.Smolder.HTML.Attributes as At
import Audio.BasePlayer (PlaybackState(..)) as BasePlayer
import Audio.SoundFont (AUDIO, Instrument, loadRemoteSoundFonts)
import Control.Monad.Eff.Class (liftEff)
import Data.Abc.PSoM.DSL (toDSL)
import Data.Abc.PSoM.Translation (toPSoM)
import Data.Abc.Parser (parse) as ABC
import Data.Array (length, fromFoldable, slice) as A
import Data.Either (Either(..), isRight)
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError(..), parse)
import Data.Euterpea.Midi.MEvent (Performance, perform1)
import Data.Foldable (traverse_)
import Data.List (List(..), null)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, instruments, read)
import Data.String (fromCharArray, toCharArray, null) as S
import Data.Tuple (Tuple(..), fst)
import JS.FileIO (FILEIO, Filespec, loadTextFile, saveTextFile)
import MultipleSelect (Event, State, foldp, initialState, view) as MS
import MultipleSelect.Dom (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, const, discard, map, max, min, pure, show, ($), (#), (<>), (+), (-), (==), (<<<))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (button, div, h1, input, label, p, span, textarea, ul, li)
import Text.Smolder.Markup (attribute, text, (#!), (!))
import View.CSS (buttonStyle, centreStyle, errorHighlightStyle, inputLabelStyle, inputStyle, labelAlignmentStyle, leftPaneStyle, leftPanelComponentStyle, rightPaneStyle, taStyle)

data Event
    = NoOp
    | Euterpea String
    | RequestFileUpload
    | RequestFileDownload
    | RequestAbcImport
    | RequestLoadFonts (Array InstrumentName)
    | FileLoaded Filespec
    | AbcLoaded Filespec
    | FontsLoaded (Array Instrument)
    | PlayerEvent Player.Event
    | InstrumentEvent MS.Event
    | Clear

type State = {
    polyphony :: String
  , instrumentChoices :: MS.State
  , fileName :: Maybe String
  , tuneResult :: Either PositionedParseError PSoM
  , performance :: Performance
  , playerState :: Player.State
}


-- | there is no tune yet
nullTune :: Either PositionedParseError PSoM
nullTune =
  Left (PositionedParseError { pos : 0, error : "" })

initialState :: State
initialState = {
    polyphony : ""
  , instrumentChoices : MS.initialState "add an instrument" instruments
  , fileName : Nothing
  , tuneResult : nullTune
  , performance : Nil
  , playerState : Player.initialState []
  }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, fileio :: FILEIO, au :: AUDIO, dom :: DOM)
foldp NoOp state =  noEffects $ state
foldp (Euterpea s) state =  onChangedEuterpea s state
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadTextFile "psominput"
         pure $ Just (FileLoaded filespec)
     ]
  }
foldp (FileLoaded filespec) state =
  onChangedFile filespec state
foldp RequestFileDownload state =
   { state: state
     , effects:
       [ do
           let
             fileName = getFileName state
             fsp = { name: fileName, contents : state.polyphony} :: Filespec
           res <- liftEff $ saveTextFile fsp
           pure $ (Just NoOp)
       ]
    }
foldp RequestAbcImport state =
  { state: state
    , effects:
      [ do
          filespec <- loadTextFile "abcinput"
          pure $ Just (AbcLoaded filespec)
      ]
   }
foldp (AbcLoaded filespec) state =
  onLoadAbcFile filespec state
foldp (RequestLoadFonts instrumentNames) state =
  let
    effects =
      [
        do  -- request the fonts are loaded
          instruments <- loadRemoteSoundFonts instrumentNames
          pure $ Just (FontsLoaded instruments)
      ]
    nilInstrumentChoices = state.instrumentChoices { selected = Nil }
    newState = state { instrumentChoices = nilInstrumentChoices }
  in
    {state: newState, effects: effects}
foldp (FontsLoaded instruments) state =
  let
    playerState = Player.initialState instruments
  in
    -- we need to react to a changed Euterpea after each instrument font loads.  This is because the user may edit
    -- the tne text to incorporate the new instrument names before loading their soundfonts
    onChangedEuterpea state.polyphony $ state { playerState = playerState }
foldp Clear state =
  onChangedEuterpea ""
    (state { polyphony = ""
           , tuneResult = nullTune
           , performance = Nil
           --, playerState = Nothing
           }
    )
foldp (PlayerEvent e) state =
  Player.foldp e state.playerState
    # mapEffects PlayerEvent
    # mapState \pst -> state { playerState = pst }
foldp (InstrumentEvent e) state =
  MS.foldp e state.instrumentChoices
    # mapEffects InstrumentEvent
    # mapState \inst -> state { instrumentChoices = inst }


-- | make sure everything is notified if the Euterpea Music changes for any reason
-- | we'll eventually have to add effects
onChangedEuterpea  :: forall e. String -> State ->  EffModel State Event ( e )
onChangedEuterpea polyphony state =
  let
    tuneResult =
      parse polyphony
    performance =
      case tuneResult of
        Right { title, music } -> perform1 music
        _ -> Nil
    parseError =
      case tuneResult of
        Right _ -> ""
        Left (PositionedParseError ppe) -> "parse error: " <> ppe.error

    newState =
      state { tuneResult = tuneResult, polyphony = polyphony, performance = performance }
  in
    case tuneResult of
      Right _ ->
        { state: newState
           , effects:
             [
              do
                pure $ Just (PlayerEvent (Player.SetPerformance performance))
            ]
        }
      Left err ->
        noEffects newState

-- | make sure everything is notified if a new file is loaded
onChangedFile :: forall e. Filespec -> State -> EffModel State Event (fileio :: FILEIO | e)
onChangedFile filespec state =
  let
    newState =
      state { fileName = Just filespec.name}
  in
    onChangedEuterpea filespec.contents newState

-- | make sure everything is notified if an ABC file is loaded for import
onLoadAbcFile :: forall e. Filespec -> State -> EffModel State Event (fileio :: FILEIO | e)
onLoadAbcFile filespec state =
  let
    psomText =
      case (ABC.parse $ filespec.contents <> "\r\n") of
        Right tune ->
          (toDSL <<< toPSoM) tune
        Left err ->
          "\"" <> filespec.name <> "\"\r\n" <> "-- error in ABC: " <> (show err)
    newState =
      state { fileName = Nothing}
  in
    onChangedEuterpea psomText newState

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


{-
debugPlayer :: State -> HTML Event
debugPlayer state =
  case state.playerState of
    Nothing ->
      do
        text ("no player state")
    Just pstate ->
      do
       text ("player melody size: " <> (show $ length pstate.melody))
-}

toInstrumentNames :: Array String -> Array InstrumentName
toInstrumentNames  =
  map (fromMaybe AcousticGrandPiano <<< read)


viewFileName :: State -> HTML Event
viewFileName state =
  case state.fileName of
    Just name ->
      text name
    _ ->
      text ""


-- | display a snippet of text with the error highlighted
viewParseError :: State -> HTML Event
viewParseError state =
  let
    -- the range of characters to display around each side of the error position
    textRange = 10
    txt = S.toCharArray state.polyphony
  in
    case state.tuneResult of
      Left (PositionedParseError pe) ->
        if (S.null state.polyphony) then
          text ""
        else
          let
            -- display a prefix of 5 characters before the error (if they're there) and a suffix of 5 after
            startPhrase =
              max (pe.pos - textRange) 0
            errorPrefix =
              A.slice startPhrase pe.pos txt
            startSuffix =
              min (pe.pos + 1) (A.length txt)
            endSuffix =
              min (pe.pos + textRange + 1) (A.length txt)
            errorSuffix =
              A.slice startSuffix endSuffix txt
            errorChar =
              A.slice pe.pos (pe.pos + 1) txt
          in
            p do
              text $ pe.error <> " - "
              text $ S.fromCharArray errorPrefix
              span ! errorHighlightStyle $ text (S.fromCharArray errorChar)
              text $ S.fromCharArray errorSuffix
      _ ->
        text ""

-- | display the intermediate Performance state
viewPerformance :: State -> HTML Event
viewPerformance state =
  do
    text $ show state.performance

-- | only display the player if we have a Melody
viewPlayer :: State -> HTML Event
viewPlayer state =
  case state.tuneResult of
    Right _ ->
      child PlayerEvent Player.view $ state.playerState
    _ ->
      text ""


loadSoundfontsButton :: State -> HTML Event
loadSoundfontsButton state =
  if (null state.instrumentChoices.selected) then
    text ""
  else
    let
      selections :: Array String
      selections = A.fromFoldable state.instrumentChoices.selected
      selectedNames = toInstrumentNames selections
    in
      div $ do
        label ! labelAlignmentStyle $ do
          text  "replace instruments:"
        button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ RequestLoadFonts selectedNames) $ text "load"

-- | is the player playing ?
isPlaying :: State -> Boolean
isPlaying state =
  let
    playbackState = state.playerState.basePlayer.playing
  in
    (playbackState == BasePlayer.PLAYING)

-- | view the menu to select from multiple instruments
-- | this is disabled if no soundfonts have yet loaded
viewInstrumentSelect :: State -> HTML Event
viewInstrumentSelect state =
  case state.playerState.basePlayer.instruments of
    [] -> text ""
    _ ->
      child InstrumentEvent MS.view $ state.instrumentChoices

viewInstrumentsLoaded :: State -> HTML Event
viewInstrumentsLoaded state =
  let
    instruments = state.playerState.basePlayer.instruments
    f inst =
      li ! At.className "msListItem" $ do
        span ! At.className "msListItemLabel" $ do
          text $ gleitzmanName $ fst inst
  in
    ul ! At.className "msList" $ do
      traverse_ f instruments

loadInstruction :: State -> String
loadInstruction state =
  case state.playerState.basePlayer.instruments of
    [] -> "wait for instruments to load"
    _ -> "loaded instruments:"

view :: State -> HTML Event
view state =
  let
    isEnabled = isRight state.tuneResult
  in
    div $ do
      h1 ! centreStyle $ text "PureScript School of Music Editor"
      -- the options and buttons on the left
      div ! leftPaneStyle $ do
        div ! leftPanelComponentStyle $ do
          label ! labelAlignmentStyle $ do
            text "load PSoM:"
          label ! inputLabelStyle ! At.className "hoverable" ! At.for "psominput" $ text "choose"
          input ! inputStyle ! At.type' "file" ! At.id "psominput" ! At.accept ".psom, .txt"
               #! onChange (const RequestFileUpload)
          button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Euterpea frereJacques) $ text "example"

        div ! leftPanelComponentStyle  $ do
          label ! labelAlignmentStyle $ do
            text "import ABC:"
          label ! inputLabelStyle ! At.className "hoverable" ! At.for "abcinput" $ text "choose"
          input ! inputStyle ! At.type' "file" ! At.id "abcinput" ! At.accept ".abc, .txt"
               #! onChange (const RequestAbcImport)

        {-}
        div ! leftPanelComponentStyle $ do
          viewFileName state
        -}
        div ! leftPanelComponentStyle  $ do
          label ! labelAlignmentStyle $ do
            -- text  "save or clear Euterpea:"
            text  "save or clear:"
          button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const RequestFileDownload) $ text "save"
          button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const Clear) $ text "clear"

        div ! leftPanelComponentStyle $ do
          viewInstrumentSelect state
          loadSoundfontsButton state
        div ! leftPanelComponentStyle $ do
          label  $ do
            text $ loadInstruction state
          viewInstrumentsLoaded state

        div ! leftPanelComponentStyle $ do
          viewPlayer state

      -- the editable text on the right
      div ! rightPaneStyle $ do
        -- p $ text $ fromMaybe "no file chosen" state.fileName
        textarea ! taStyle ! At.cols "70" ! At.rows "15"
          ! At.spellcheck "false" ! At.autocomplete "false" ! At.autofocus "true"
            ! At.value state.polyphony  ! At.wrap "hard"  ! (attribute "key" "polyphony" )
          #! onInput (\e -> Euterpea (targetValue e) ) $ text ""
        viewParseError state



frereJacques :: String
frereJacques =
  "\"Frere Jacques\"\r\n" <>
  "-- More examples at https://github.com/newlandsvalley/purescript-school-of-music/tree/master/editor/examples \r\n" <>
  "Let \r\n" <>
  "    ln1 = Line Note qn G 3, Note qn A 3, Note qn B 3, Note qn G 3  \r\n" <>
  "    ln2 = Line Note qn B 3, Note qn C 4, Note hn D 4 \r\n" <>
  "    ln3 = Line Note en D 4, Note en E 4, Note en D 4, Note en C 4, Note qn B 3, Note qn G 3 \r\n" <>
  "    ln4 = Line Note qn G 3, Note qn D 3, Note hn G 3 \r\n" <>
  "    rest = Rest wn \r\n" <>
  "In \r\n" <>
  "  Par \r\n" <>
  "     Instrument acoustic_bass ( Transpose -12 ( Seq ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )) \r\n" <>
  "     Instrument vibraphone ( Transpose 12 ( Seq rest rest ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )) \r\n" <>
  "     Instrument acoustic_grand_piano ( Seq rest rest rest rest ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )"
