module App where

import Audio.SoundFont (AUDIO, Instrument, loadRemoteSoundFonts)
import Audio.Euterpea.Player as Player
import Audio.BasePlayer (PlaybackState(..)) as BasePlayer
import Data.Array (length, slice) as A
import Data.Either (Either(..), isRight)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray, toCharArray, null) as S
import JS.FileIO (FILEIO)
import Prelude (const, bind, discard, max, min, pure, show, ($), (#), (<>), (+), (-), (==))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (onClick, onInput, targetValue)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (button, div, h1, label, p, span, textarea)
import Text.Smolder.HTML.Attributes as At
import Text.Smolder.Markup (text, (#!), (!))
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError(..), parse)
import Data.Euterpea.Midi.MEvent (Performance, perform1)
import Data.Midi.Instrument (InstrumentName)
import View.CSS (buttonStyle, centreStyle, errorHighlightStyle, labelAlignmentStyle, leftPaneStyle, leftPanelComponentStyle,
  rightPaneStyle, taStyle)
import Network.HTTP.Affjax (AJAX)

data Event
    = NoOp
    | Euterpea String
    | RequestLoadFonts (Array InstrumentName)
    | FontsLoaded (Array Instrument)
    | PlayerEvent Player.Event
    | Example String
    | Clear

type State = {
    polyphony :: String
  -- , fileName :: Maybe String
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
  , tuneResult : nullTune
  , performance : List.Nil
  , playerState : Player.initialState []
  }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, fileio :: FILEIO, au :: AUDIO)
foldp NoOp state =  noEffects $ state
foldp (Euterpea s) state =  onChangedEuterpea s state
foldp (RequestLoadFonts instrumentNames) state =
  let
    effects =
      [
        do  -- request the fonts are loaded
          instruments <- loadRemoteSoundFonts instrumentNames
          pure $ Just (FontsLoaded instruments)
      ]
  in
    {state: state, effects: effects}
foldp (FontsLoaded instruments) state =
  let
    playerState = Player.initialState instruments
  in
    -- we need to react to a changed Euterpea after each instrument font loads.  This is because the user may edit
    -- the tne text to incorporate the new instrument names before loading their soundfonts
    onChangedEuterpea state.polyphony $ state { playerState = playerState }
foldp (Example example) state =  onChangedEuterpea example state
foldp Clear state =
  onChangedEuterpea ""
    (state { polyphony = ""
           , tuneResult = nullTune
           , performance = List.Nil
           }
    )
foldp (PlayerEvent e) state =
  Player.foldp e state.playerState
    # mapEffects PlayerEvent
    # mapState \pst -> state { playerState = pst }

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
        _ -> List.Nil
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

-- | is the player playing ?
isPlaying :: State -> Boolean
isPlaying state =
  let
    playbackState = state.playerState.basePlayer.playing
  in
    (playbackState == BasePlayer.PLAYING)

sampleButtons :: State -> HTML Event
sampleButtons state =
  case state.playerState.basePlayer.instruments of
   [] -> text "loading instruments..."
   _ ->
     div ! leftPanelComponentStyle  $ do
       label ! labelAlignmentStyle $ do
         -- text  "save or clear Euterpea:"
         text  "clear Euterpea:"
       -- button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const RequestFileDownload) $ text "save"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const Clear) $ text "clear"
       label ! labelAlignmentStyle $ do
         text  "simple line:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example1) $ text "example 1"
       label ! labelAlignmentStyle $ do
         text  "simple chords:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example2) $ text "example 2"
       label ! labelAlignmentStyle $ do
         text  "change tempo:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example3) $ text "example 3"
       label ! labelAlignmentStyle $ do
         text  "polska voice 1:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example4) $ text "example 4"
       label ! labelAlignmentStyle $ do
         text  "polska voice 2:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example5) $ text "example 5"
       label ! labelAlignmentStyle $ do
         text  "polska polyphony:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example6) $ text "example 6"
       label ! labelAlignmentStyle $ do
         text  "frere Jacques:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example7) $ text "example 7"
       label ! labelAlignmentStyle $ do
         text  "loudness:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example8) $ text "example 8"
       label ! labelAlignmentStyle $ do
         text  "diminuendo:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example9) $ text "example 9"
       label ! labelAlignmentStyle $ do
         text  "accelerate/decelerate:"
       button ! (buttonStyle true) ! At.className "hoverable" #! onClick (const $ Example example10) $ text "example 10"


view :: State -> HTML Event
view state =
  let
    isEnabled = isRight state.tuneResult
  in
    div $ do
      h1 ! centreStyle $ text "PSoM Samples"
      -- the options and buttons on the left
      div ! leftPaneStyle $ do
        sampleButtons state

        div ! leftPanelComponentStyle $ do
          viewPlayer state

      -- the editable text on the right
      div ! rightPaneStyle $ do
        -- p $ text $ fromMaybe "no file chosen" state.fileName
        textarea ! taStyle ! At.cols "70" ! At.rows "15" ! At.value state.polyphony
          ! At.spellcheck "false" ! At.autocomplete "false" ! At.autofocus "true"
          #! onInput (\e -> Euterpea (targetValue e) ) $ text ""
        viewParseError state

      div! rightPaneStyle $ do
        viewPerformance state


-- | some examples
example1 :: String
example1 = "\"line\"\r\n Line Note qn C 3, Note qn D 3, Note hn E 3, Note hn F 3"

example2 :: String
example2 = "\"Chord\"\r\n Line Chord [ Note dhn A 4, Note dhn C 4, Note dhn E 3 ], Note qn B 4, Note hn A 4,\r\n" <>
           "    Note hn G 4, Chord [ Note wn E 5, Note wn B 4, Note wn G 4, Note wn E 4 ]\r\n"

example3 :: String
example3 =
  "\"Change Tempo\"\r\n" <>
  "Let \r\n" <>
  "  ln1 = Line Note qn G 3, Note qn A 3, Note qn B 3, Note qn G 3  \r\n" <>
  " In \r\n" <>
  "   Par \r\n" <>
  "     Instrument acoustic_bass ( Transpose -12 ( Seq ln1 ln1 ln1 ln1 )) \r\n" <>
  "     Instrument vibraphone ( Tempo 1/2 ( Seq ln1 ln1 ))"

example4 :: String
example4 =
  "\"Polska Voice 1\"\r\n" <> example4Music

example4Music :: String
example4Music =
  "Let \r\n" <>
  "    v1 = \r\n" <>
  "       Line Note qn Bf 5, Note qn A 5, Note qn G 5, \r\n" <>
  "         Note sn D 5, Note sn E 5, Note sn Fs 5, Note sn G 5, Note en A 5, \r\n" <>
  "         Note sn A 5, Note sn C 6, Note en Bf 5, Note en A 5, \r\n" <>
  "         Note en G 5, Note sn Ef 6, Note sn D 6, Note en C 6, Note en Bf 5, \r\n" <>
  "         Note en A 5, Note en G 5, \r\n" <>
  "         Note sn Fs 5, Note sn G 5, Note sn A 5 \r\n" <>
  "    end1 = \r\n" <>
  "         Line Note sn G 5, Note en Fs 5, Note en A 5, Note en G 5,  Note en A 5 \r\n" <>
  "    end2 = \r\n" <>
  "         Line Note sn Fs 5, Note qn G 5, Note qn G 4 \r\n" <>
  " In \r\n" <>
  "    Seq v1 end1 v1 end2 v1 end1 v1 end2"

example5 :: String
example5 =
  "\"Polska Voice 2\"\r\n" <> example5Music

example5Music :: String
example5Music =
  "Let \r\n" <>
  "    v2 = \r\n" <>
  "      Line Note qn G 5, Note qn Fs 5, Note qn D 5, \r\n" <>
  "        Note en A 4, Note en D 5, Note en Fs 5, \r\n" <>
  "        Note sn Fs 5, Note sn A 5, Note en G 5, Note en Fs 5, \r\n" <>
  "        Note en D 5, Note sn C 6, Note sn Bf 5, Note en A 5, Note en G 5, \r\n" <>
  "        Note en Fs 5, Note en D 5, \r\n" <>
  "        Note sn D 5, Note sn E 5, Note sn Fs 5, Note sn D 5 \r\n" <>
  "    end1 = \r\n" <>
  "     Line Note en D 5, Note en Fs 5, Note en D 5, Note en Fs 5 \r\n" <>
  "    end2 = \r\n" <>
  "     Line Note qn Bf 4, Note qn G 4 \r\n" <>
  "  In \r\n" <>
  "    Instrument vibraphone ( Seq v2 end1 v2 end2 v2 end1 v2 end2 )"


example6 :: String
example6 =
  "\"Polska\"\r\n" <>
  "Par \r\n" <>
    example4Music <> "\r\n" <> example5Music

example7 :: String
example7 =
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
  "     Seq rest rest rest rest ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 "

example8 :: String
example8 =
  "\"Loudness\"\r\n" <>
  "Let \r\n" <>
  "  ln1 = Instrument acoustic_bass (Line Note qn G 2, Note qn A 2, Note qn B 2, Note qn G 2 ) \r\n" <>
  "In \r\n" <>
  "  Seq \r\n" <>
  "    PhraseAtts Loudness 120 ( Seq ln1 ) \r\n" <>
  "    PhraseAtts Loudness 90 ( Seq ln1 ) \r\n" <>
  "    PhraseAtts Loudness 60 ( Seq ln1 ) \r\n" <>
  "    PhraseAtts Loudness 30 ( Seq ln1 ) \r\n" <>
  "    PhraseAtts StdLoudness PPP ( Seq ln1 ) \r\n" <>
  "    PhraseAtts StdLoudness PP ( Seq ln1 ) \r\n" <>
  "    PhraseAtts StdLoudness MF ( Seq ln1 ) \r\n" <>
  "    PhraseAtts StdLoudness FFF ( Seq ln1 ) \r\n"

example9 :: String
example9 =
  "\"Diminuendo\"\r\n" <>
  "Let \r\n" <>
  "  ln1 = Line Note qn G 3, Note qn A 3, Note qn B 3, Note qn C 4, " <>
  "  Note qn D 4, Note qn E 4, Note qn Fs 4, Note qn G 4  \r\n" <>
  "In \r\n" <>
  "  Seq \r\n" <>
  "    PhraseAtts Diminuendo 7/8 ( Seq ln1 ) \r\n"

example10 :: String
example10 =
  "\"Accelerate/Decelerate\"\r\n" <>
  "Let \r\n" <>
  "  ln1 = Line Note qn G 3, Note qn A 3, Note qn B 3, Note qn C 4, " <>
  "  Note qn D 4, Note qn E 4, Note qn Fs 4, Note qn G 4  \r\n" <>
  "In \r\n" <>
  "  Seq \r\n" <>
  "    ln1 \r\n" <>
  "    PhraseAtts Ritardando 1/2 ( Seq ln1 ) \r\n"  <>
  "    PhraseAtts Accelerando 1/2 ( Seq ln1 ) \r\n"
