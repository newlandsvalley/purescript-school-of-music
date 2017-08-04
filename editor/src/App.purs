module App where

import Audio.SoundFont (AUDIO, LoadResult, loadRemoteSoundFonts)
import Audio.Euterpea.Player as Player
import Audio.BasePlayer (PlaybackState(..)) as BasePlayer
import MultipleSelect (Event, State, foldp, initialState, view) as MS
import MultipleSelect.Dom (DOM)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (length, fromFoldable, slice) as A
import Data.Either (Either(..), isLeft, isRight)
import Data.List (List(..), null, (:))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.Monoid (mempty)
import Data.Map (Map(..), empty, fromFoldable, insert, lookup, keys)
import Data.Tuple (Tuple(..))
import Data.String (fromCharArray, toCharArray)
import View.CSS
import FileIO.FileIO (FILEIO, Filespec, loadTextFile, saveTextFile)
import Prelude (bind, const, discard, id, max, min, negate, not, pure, show, ($), (#), (<>), (+), (-), (==), (<<<))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (DOMEvent, onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (button, canvas, div, h1, input, label, p, span, select, textarea, ul, li)
import Text.Smolder.HTML.Attributes as At
import Text.Smolder.Markup (text, (#!), (!), (!?))
import Data.Euterpea.Music
import Data.Euterpea.Music1 (Music1, Note1(..))
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError(..), parse)
import Data.Euterpea.Midi.MEvent (Performance, MEvent(..), perform1)
import Data.Euterpea.Instrument (InstrumentMap(..), instruments)


-- import Debug.Trace (trace, traceShow, traceShowM)


data Event
    = NoOp
    | Euterpea String
    | RequestFileUpload
    | RequestFileDownload
    | RequestLoadFonts
    | FileLoaded Filespec
    | FontLoaded LoadResult
    | PlayerEvent Player.Event
    | InstrumentEvent MS.Event
    | Clear

type State = {
    polyphony :: String
  , instrumentChoices :: MS.State
  , availableInstruments :: InstrumentMap
  , fileName :: Maybe String
  , tuneResult :: Either PositionedParseError PSoM
  , performance :: Performance
  , playerState :: Maybe Player.State
}


-- | there is no tune yet
nullTune :: Either PositionedParseError PSoM
nullTune =
  Left (PositionedParseError { pos : 0, error : "" })

-- | hard-code the instrument map while we're still developing
initialInstruments :: InstrumentMap
initialInstruments =
  fromFoldable
    ( Tuple "acoustic_grand_piano" 0
    : Tuple "vibraphone" 1
    : Tuple "acoustic_bass" 2
    : Nil
    )

initialState :: State
initialState = {
    polyphony : ""
  , instrumentChoices : MS.initialState "add an instrument" instruments
  , availableInstruments : initialInstruments
  , fileName : Nothing
  , tuneResult : nullTune
  , performance : Nil
  , playerState : Nothing
  }

{-}
initialState :: State
initialState = {
    polyphony : frereJacques
  , instrumentChoices : MS.initialState "add an instrument" instruments
  , availableInstruments : initialInstruments
  , fileName : Nothing
  , tuneResult : parse frereJacques
  , performance : initialPerformance
  , playerState : Just (Player.initialState initialInstruments)
  }

initialPerformance :: Performance
initialPerformance =
  case parse frereJacques of
    Right { title, music } -> perform1 music
    _ -> Nil
-}



foldp :: Event -> State -> EffModel State Event (fileio :: FILEIO, au :: AUDIO, dom :: DOM)
foldp NoOp state =  noEffects $ state
foldp (Euterpea s) state =  onChangedEuterpea s state
foldp RequestFileUpload state =
 { state: state
   , effects:
     [ do
         filespec <- loadTextFile
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
foldp RequestLoadFonts state =
  let
    selections :: Array String
    selections = A.fromFoldable state.instrumentChoices.selected
    effects =
      [
        do  -- request the fonts are loaded
          msg <- loadRemoteSoundFonts selections
          pure $ Just (FontLoaded msg)
      ]
    nilInstrumentChoices = state.instrumentChoices { selected = Nil }
    newState = state { instrumentChoices = nilInstrumentChoices, availableInstruments = empty }
  in
    {state: newState, effects: effects}
foldp (FontLoaded loadResult) state =
  let
    availableInstruments = insert loadResult.instrument loadResult.channel state.availableInstruments
    playerState = Just (Player.initialState availableInstruments)
  in
    -- noEffects $ state { availableInstruments = availableInstruments, playerState = playerState }
    -- we need to react to a changed Euterpea after each instrument font loads.  This is because the user may edit
    -- the tine text to incorporate the new instrument names before loading their soundfonts
    onChangedEuterpea state.polyphony $ state { availableInstruments = availableInstruments, playerState = playerState }
foldp Clear state =
  onChangedEuterpea ""
    (state { polyphony = ""
           , tuneResult = nullTune
           , performance = Nil
           , playerState = Nothing
           }
    )
foldp (PlayerEvent e) state =
  case state.playerState of
    Just pstate ->
      Player.foldp e pstate
        # mapEffects PlayerEvent
        # mapState \pst -> state { playerState = Just pst }
    _ ->
      noEffects state
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

    newState =
      state { tuneResult = tuneResult, polyphony = polyphony, performance = performance }
  in
    case tuneResult of
      Right _ ->
        { state: newState { playerState = Just (Player.initialState state.availableInstruments)}
           , effects:
             [
              do
                pure $ Just (PlayerEvent (Player.SetPerformance performance))
            ]
        }
      Left err ->
        noEffects $ newState { playerState = Nothing }

-- | make sure everything is notified if a new file is loaded
onChangedFile :: forall e. Filespec -> State -> EffModel State Event (fileio :: FILEIO | e)
onChangedFile filespec state =
  let
    newState =
      state { fileName = Just filespec.name}
  in
    onChangedEuterpea filespec.contents newState


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


viewFileName :: State -> HTML Event
viewFileName state =
  case state.fileName of
    Just name ->
      text name
    _ ->
      mempty


-- | display a snippet of text with the error highlighted
viewParseError :: State -> HTML Event
viewParseError state =
  let
    -- the range of characters to display around each side of the error position
    textRange = 10
    txt = toCharArray state.polyphony
  in
    case state.tuneResult of
      Left (PositionedParseError pe) ->
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
              text $ fromCharArray errorPrefix
              span ! errorHighlightStyle $ text (fromCharArray errorChar)
              text $ fromCharArray errorSuffix
      _ ->
        mempty

-- | display the intermediate Performance state
viewPerformance :: State -> HTML Event
viewPerformance state =
  do
    text $ show state.performance

-- | only display the player if we have a Melody
viewPlayer :: State -> HTML Event
viewPlayer state =
  case state.playerState of
    Just pstate ->
      child PlayerEvent Player.view $ pstate
    _ ->
      mempty

loadSoundfontsButton :: State -> HTML Event
loadSoundfontsButton state =
  if (null state.instrumentChoices.selected) then
    mempty
  else
    div $ do
      label ! labelAlignmentStyle $ do
        text  "replace instruments:"
      button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const RequestLoadFonts) $ text "load"

-- | is the player playing ?
isPlaying :: State -> Boolean
isPlaying state =
  case state.playerState of
    Just ps ->
      let
        playbackState = ps.basePlayer.playing
      in
        (playbackState == BasePlayer.PLAYING)
    _ -> false

viewInstrumentSelect :: State -> HTML Event
viewInstrumentSelect state =
  child InstrumentEvent MS.view $ state.instrumentChoices

viewInstrumentsLoaded :: State -> HTML Event
viewInstrumentsLoaded state =
  let
    instruments = keys state.availableInstruments
    channel s = fromMaybe (-1) $ lookup s state.availableInstruments
    f s =
      li ! At.className "msListItem" $ do
        span ! At.className "msListItemLabel" $ do
          text $ s <> ": channel " <> (show $ channel s)
  in
    ul ! At.className "msList" $ do
      traverse_ f instruments


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
          label ! inputLabelStyle ! At.className "hoverable" ! At.for "fileinput" $ text "choose"
          input ! inputStyle ! At.type' "file" ! At.id "fileinput" ! At.accept ".psom, .txt"
               #! onChange (const RequestFileUpload)
          button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Euterpea frereJacques) $ text "example"

        div ! leftPanelComponentStyle $ do
          viewFileName state
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
          label ! labelAlignmentStyle $ do
            text "loaded instruments:"
          viewInstrumentsLoaded state

        div ! leftPanelComponentStyle $ do
          viewPlayer state


      -- the editable text on the right
      div ! rightPaneStyle $ do
        -- p $ text $ fromMaybe "no file chosen" state.fileName
        textarea ! taStyle ! At.cols "70" ! At.rows "15" ! At.value state.polyphony
          ! At.spellcheck "false" ! At.autocomplete "false" ! At.autofocus "true"
          #! onInput (\e -> Euterpea (targetValue e) ) $ mempty
        viewParseError state

      div! rightPaneStyle $ do
        viewPerformance state

frereJacques :: String
frereJacques =
  "\"Frere Jacques\"\r\n" <>
  "Let \r\n" <>
  "    ln1 = Line Note qn G 3, Note qn A 3, Note qn B 3, Note qn G 3  \r\n" <>
  "    ln2 = Line Note qn B 3, Note qn C 4, Note hn D 4 \r\n" <>
  "    ln3 = Line Note en D 4, Note en E 4, Note en D 4, Note en C 4, Note qn B 3, Note qn G 3 \r\n" <>
  "    ln4 = Line Note qn G 3, Note qn D 3, Note hn G 3 \r\n" <>
  "    rest = Line Rest wn \r\n" <>
  "In \r\n" <>
  "  Par \r\n" <>
  "     Instrument acoustic_bass ( Transpose -12 ( Seq ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )) \r\n" <>
  "     Instrument vibraphone ( Seq rest rest ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )\r\n" <>
  "     Instrument acoustic_grand_piano ( Seq rest rest rest rest ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )"
