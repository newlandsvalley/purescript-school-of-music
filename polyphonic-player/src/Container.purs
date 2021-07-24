module Container where

import Prelude

import Audio.Euterpea.Playable (PlayablePSoM(..))
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Text.Parsing.StringParser (ParseError)
import Data.Abc (AbcTune)
import Data.Abc.Metadata (getTitle)
import Data.Abc.PSoM.Polyphony (generateDSL, generateDSL')
import Data.Abc.Voice (getVoiceMap)
import Data.Array (cons, head, null, fromFoldable, mapWithIndex)
import Data.Either (Either(..), either, hush)
import Data.Euterpea.DSL.Parser (PSoM, parse)
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Map (Map, empty, keys, lookup, size, values)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, gleitzmanNames, readGleitzman)
import Data.Set (toUnfoldable) as Set
import Data.String (stripPrefix)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (fst)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.EditorComponent as ED
import Halogen.FileInputComponent as FIC
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MultipleSelectComponent as MSC
import Halogen.PlayerComponent as PC
import Halogen.SimpleButtonComponent as Button
import VexFlow.Score (Renderer, clearCanvas, renderFinalTune, initialiseCanvas) as Score
import VexFlow.Types (Config)
import Type.Proxy (Proxy(..))


type State =
  { instruments :: Array Instrument
  , tuneResult :: Either ParseError AbcTune
  , voicesMap :: Map String AbcTune
  , currentVoice :: Maybe String
  , ePsom  :: Either ParseError PSoM
  , fileName :: Maybe String
  , vexRenderer :: Maybe Score.Renderer
  , playAllVoices :: Boolean
  }

data Action =
    Init
  | HandleABCFile FIC.Message
  | HandleClearButton Button.Message
  | HandleNewTuneText ED.Message
  | HandleTuneIsPlaying PC.Message
  | NewInstrumentsSelection MSC.Message
  | HandleChangeVoice String
  | HandleChangePlaybackMode Boolean

voiceNamePrefix :: String 
voiceNamePrefix = "voice: "

vexConfig :: Config
vexConfig =
  { parentElementId : "vexflow"
  , width : 1300
  , height : 700
  , scale : 0.8
  , isSVG : true
  , titled : false
  }

abcFileInputCtx :: FIC.Context
abcFileInputCtx =
  { componentId : "abcinput"
  , isBinary : false
  , prompt : "load"
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
nullAbcTune :: Either ParseError AbcTune
nullAbcTune =
  Left { error : "", pos : 0 }

nullPsomTune :: Either ParseError PSoM
nullPsomTune =
  Left { error : "", pos : 0 }

emptyTune :: AbcTune 
emptyTune = 
  { headers : Nil, body: Nil }

parseError :: Either ParseError AbcTune -> String
parseError tuneResult =
  case tuneResult of
    Right _ -> "no errors"
    Left { error, pos } -> "parse error: " <> error <> " at " <> (show pos)

type ChildSlots =
  ( editor :: ED.Slot Unit
  , abcfile :: FIC.Slot Unit
  , instrument :: MSC.Slot Unit
  , clear :: Button.Slot Unit
  , player :: (PC.Slot PlayablePSoM) Unit
  )

_editor = Proxy :: Proxy "editor"
_abcfile = Proxy :: Proxy "abcfile"
_instrument = Proxy :: Proxy "instrument"
_clear = Proxy :: Proxy "clear"
_player = Proxy :: Proxy "player"

component :: forall q i o. H.Component q i o Aff
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
    , tuneResult: nullAbcTune
    , voicesMap : empty
    , currentVoice : Nothing
    , ePsom: nullPsomTune
    , fileName: Nothing
    , vexRenderer: Nothing
    , playAllVoices: true
    }

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o Aff Unit
  handleAction = case _ of
    Init -> do
      instruments <- H.liftAff $  loadRemoteSoundFonts  [AcousticGrandPiano, Vibraphone, Viola]
      renderer <- H.liftEffect $ Score.initialiseCanvas vexConfig
      _ <- H.modify (\st -> st { instruments = instruments
                               , vexRenderer = Just renderer } )
      pure unit
    HandleABCFile (FIC.FileLoaded filespec) -> do

      _ <- H.modify (\st -> st { fileName = Just filespec.name } )
      _ <- H.tell _editor unit $ (ED.UpdateContent filespec.contents)
      _ <- H.tell _player unit PC.StopMelody
      pure unit
    HandleClearButton (Button.Toggled _) -> do
      state <- H.get
      _ <- H.modify (\st -> st { fileName = Nothing
                               , tuneResult = nullAbcTune
                               , voicesMap = empty :: Map String AbcTune
                               , currentVoice = Nothing
                               , ePsom = nullPsomTune
                               , playAllVoices = true
                               } )
      _ <- H.tell _editor unit $ (ED.UpdateContent "")
      _ <- H.tell _player unit $ PC.StopMelody
      case state.vexRenderer of 
        Just renderer -> do
          _ <- H.liftEffect $ Score.clearCanvas renderer
          pure unit
        _ -> 
          pure unit
    HandleNewTuneText (ED.TuneResult eTuneResult) -> 
      case eTuneResult of
        Right tune -> do
          state <- H.get
          let 
            voicesMap = getVoiceMap tune
            voiceNames = getVoiceNames voicesMap
            currentVoice = head voiceNames
            ePsom = generatePsom state currentVoice tune          
            -- render the score 
          _ <- displayRenderdScore state.vexRenderer currentVoice voicesMap tune
          _ <- refreshPlayerState ePsom
          _ <- H.modify (\st -> st { tuneResult = eTuneResult
                                   , voicesMap = voicesMap
                                   , currentVoice = currentVoice
                                   , ePsom = ePsom
                                   , playAllVoices = true } )
          pure unit
        Left _ -> 
          pure unit
    NewInstrumentsSelection (MSC.CommittedSelections pendingInstrumentNames) -> do
      let
        f acc s =
          case readGleitzman s of
            Just inst -> cons inst acc
            _ -> acc
        instrumentNames :: Array InstrumentName
        instrumentNames = foldl f [] pendingInstrumentNames
      instruments <- H.liftAff $ loadRemoteSoundFonts instrumentNames
      _ <- H.tell _player unit $ (PC.SetInstruments instruments)
      _ <- H.modify (\st -> st { instruments = instruments})
      pure unit
    HandleTuneIsPlaying (PC.IsPlaying _) -> do
      -- we ignore this message, but if we wanted to we could
      -- disable any button that can alter the editor contents whilst the player
      -- is playing and re-enable when it stops playing
      -- _ <- H.query _editor unit $ H.tell (ED.UpdateEnabled (not p))
      -- _ <- H.query _psomfile unit $ H.tell (FIC.UpdateEnabled (not p))
      -- _ <- H.query _abcfile unit $ H.tell (FIC.UpdateEnabled (not p))
      -- _ <- H.query _clear unit $ H.tell (Button.UpdateEnabled (not p))
      -- _ <- H.query _sample unit $ H.tell (Button.UpdateEnabled (not p))
      pure unit
    HandleChangeVoice voice -> do          
      state <- H.get
      -- strip the 'voice: ' prefix from the voice name we get from the menu
      let 
        currentVoice :: Maybe String
        currentVoice = stripPrefix (Pattern voiceNamePrefix) voice
        tune :: AbcTune
        tune = either (const emptyTune) identity state.tuneResult
      _ <- displayRenderdScore state.vexRenderer currentVoice state.voicesMap tune
      reloadPlayer (state { currentVoice = currentVoice} )
      _ <- H.modify (\st -> st { currentVoice = currentVoice })
      pure unit
    HandleChangePlaybackMode playMode -> do     
      state <- H.get
      let 
        newState = state { playAllVoices = playMode }
      reloadPlayer (newState)
      _ <- H.put newState
      pure unit


  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render state = HH.div_
    [ HH.h1
      [HP.class_ (H.ClassName "center") ]
      [HH.text "Polyphonic ABC Player"]
    , HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        -- load and clear
        HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent") ]
         [  HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "ABC:" ]
         , HH.slot _abcfile unit (FIC.component abcFileInputCtx) unit HandleABCFile
         , HH.slot _clear unit (Button.component "clear") unit HandleClearButton
         ]
        -- render voice menus if we have more than 1 voice
      , renderPossibleVoiceMenus state
        -- load instruments
      , HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [
            HH.slot _instrument unit
               (MSC.component multipleSelectCtx initialMultipleSelectState) unit NewInstrumentsSelection
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
            HH.slot _editor unit ED.component unit HandleNewTuneText
          ]
      , renderScore state
      --, renderDebug state
    ]

  renderPlayer :: State -> H.ComponentHTML Action ChildSlots Aff
  renderPlayer state =
    case state.ePsom of
      Right psom ->
        HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [
             HH.slot _player unit (PC.component (PlayablePSoM psom) state.instruments) unit (HandleTuneIsPlaying)
          ]
      Left _ ->
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

  -- we only render these menus if we have more than 1 voice
  renderPossibleVoiceMenus :: State -> H.ComponentHTML Action ChildSlots Aff 
  renderPossibleVoiceMenus state = 
    if (size state.voicesMap <= 1) then
      HH.div_
        []
    else
      let 
        voiceNames = Set.toUnfoldable (keys state.voicesMap)
        currentVoice = fromMaybe "none" state.currentVoice
      in
        HH.div_ 
          [ renderVoiceMenu currentVoice voiceNames
          , renderPlaybackMenu state.playAllVoices
          ]
  
  renderVoiceMenu :: String -> Array String ->  H.ComponentHTML Action ChildSlots Aff
  renderVoiceMenu currentVoice voices =   
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ HH.label
         [ HP.class_ (H.ClassName "labelAlignment") ]
         [ HH.text "voice preferences: " ]
      , HH.div_ $ mapWithIndex (addVoiceRadio currentVoice) voices
      ]

  addVoiceRadio :: String -> Int -> String -> H.ComponentHTML Action ChildSlots Aff
  addVoiceRadio selected id voiceName =
    HH.div_
      [
        HH.input
          [ HP.type_ HP.InputRadio
          , HP.class_ (H.ClassName "voice-radio")
          , HP.name "voice-radio"
          , HP.value label
          , HP.id voiceId
          , HP.checked isChecked
          , HE.onValueInput HandleChangeVoice
          ]
      , HH.label 
        [ HP.for voiceId
        , HP.class_ (H.ClassName "radio-label")
        ] 
        [ HH.text ("show " <> label) ]
      ]
    where 
      isChecked = selected == voiceName
      label =  (voiceNamePrefix <> voiceName)
      voiceId = "voice" <> show id


  renderPlaybackMenu :: Boolean -> H.ComponentHTML Action ChildSlots Aff
  renderPlaybackMenu currentPlayback =   
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ 
        HH.div_ $ map (addPlaybackRadio currentPlayback) [true, false]
      ]  

  addPlaybackRadio :: Boolean -> Boolean -> H.ComponentHTML Action ChildSlots Aff
  addPlaybackRadio selected playMode =
    HH.div_
      [
        HH.input
          [ HP.type_ HP.InputRadio
          , HP.class_ (H.ClassName "playback-radio")
          , HP.name "playback-radio"
          , HP.value value
          , HP.id label
          , HP.checked isChecked
          , HE.onValueInput (toBool >>> HandleChangePlaybackMode)
          ]
      , HH.label 
        [ HP.for label
        , HP.class_ (H.ClassName "radio-label")
        ] 
        [ HH.text label ]
      ]
    where 
      isChecked = selected == playMode
      label = 
        if playMode then 
          "play all voices"
        else 
          "play one voice"
      value = 
        if playMode then 
          "true"
        else 
          "false"
      toBool str = (str == "true") 
  {-}
  renderScore :: ∀ m
    . MonadAff m
    => State
    -> H.ComponentHTML Action ChildSlots m
  -}
  renderScore :: State -> H.ComponentHTML Action ChildSlots Aff
  renderScore state =
    HH.div
      [ HP.id "score"]
      [ renderTuneTitle state
        , HH.div
           [ HP.class_ (H.ClassName "canvasDiv")
           , HP.id "vexflow"
           ] []
      ]   

  {-}
  renderTuneTitle :: ∀ m
    . MonadAff m
    => State
    -> H.ComponentHTML Action ChildSlots m
  -}
  renderTuneTitle :: State -> H.ComponentHTML Action ChildSlots Aff
  renderTuneTitle state =
    let 
      voiceName = maybe "" (\v -> " (voice " <> v <> ")") state.currentVoice
    in
      case (hush state.tuneResult >>= getTitle) of
        Just title ->
          HH.h2
            [HP.id "tune-title" ]
            [HH.text (title <> voiceName)]
        _ ->
          HH.text ""
 
  {-
  renderDebug :: State -> H.ComponentHTML Action ChildSlots Aff 
  renderDebug state = 
    HH.div_
      [ HH.text ("current voice:" <> (fromMaybe "none" state.currentVoice)) ]
  -}

-- helpers
-- | get the file name
getFileName :: State -> String
getFileName state =
  case state.fileName of
    Just name ->
      name
    _ ->
      case state.tuneResult of
        Right abcTune ->
          let 
            title = fromMaybe "untitle" $ getTitle abcTune 
          in
            title <> ".abc"
        _ ->
          "untitled.abc"

-- Generate the possibly polyphonic PSoM DSL
-- if the user selects just one voice or there is only once voice in the tune anyway 
-- then this defaults to a monophonic tune DSL
generatePsom :: State -> Maybe String -> AbcTune -> Either ParseError PSoM
generatePsom state mCurrentVoice tune =     
  let
    instrumentNames = map fst state.instruments
    voicesMap = getVoiceMap tune
    dsl = 
      -- polyphony
      if state.playAllVoices then
        let
          voices = fromFoldable (values voicesMap)
          title = fromMaybe "unnamed" $ getTitle tune 
        in
          generateDSL' voices instrumentNames title
      -- monophony
      else
        let 
          currentVoice = fromMaybe "nothing" mCurrentVoice
          -- _ = spy "current voice" currentVoice
          voiceTune = fromMaybe tune $ lookup currentVoice voicesMap 
        in
          generateDSL voiceTune instrumentNames
  in 
    parse dsl

getVoiceNames :: Map String AbcTune -> Array String 
getVoiceNames voicesMap = 
  fromFoldable (keys voicesMap)    


displayRenderdScore :: ∀ o.
       Maybe Score.Renderer
    -> Maybe String 
    -> Map String AbcTune 
    -> AbcTune
    -> H.HalogenM State Action ChildSlots o Aff Unit
displayRenderdScore mRenderer mCurrentVoice voicesMap tune =
  case mRenderer of 
    Nothing -> 
      pure unit
    Just renderer -> do
      -- render the whole tune if only one voice
      if (size voicesMap) <= 1 then do
        _ <- H.liftEffect $ Score.clearCanvas $ renderer
        _ <- H.liftEffect $ Score.renderFinalTune vexConfig renderer tune
        pure unit
      -- otherwise render the selected voice
      else       
        let 
          currentVoice = fromMaybe "nothing" mCurrentVoice
          -- _ = spy "current voice" currentVoice
          voiceTune = fromMaybe tune $ lookup currentVoice voicesMap 
        in do
          _ <- H.liftEffect $ Score.clearCanvas $ renderer
          _ <- H.liftEffect $ Score.renderFinalTune vexConfig renderer voiceTune
          pure unit

reloadPlayer ::  ∀ o.
       State 
    -> H.HalogenM State Action ChildSlots o Aff Unit
reloadPlayer state =
  case state.tuneResult of
    Right tune -> do
      let 
        -- voicesMap = getVoiceMap tune
        -- voiceNames = getVoiceNames voicesMap
        ePsom = generatePsom state state.currentVoice tune
      refreshPlayerState ePsom
    _ ->
      pure unit
  

-- refresh the state of the player by passing it the tune result
-- (if it had parsed OK)
refreshPlayerState :: ∀ o.
       Either ParseError PSoM
    -> H.HalogenM State Action ChildSlots o Aff Unit
refreshPlayerState tuneResult = do
  _ <- either
     {-}
     (\_ -> H.query _player unit $ H.tell PC.StopMelody)
     (\psom -> H.query _player unit $ H.tell (PC.HandleNewPlayable (PlayablePSoM psom)))
     tuneResult
     -}
     (\_ -> H.tell _player unit PC.StopMelody)
     (\psom -> H.tell _player unit (PC.HandleNewPlayable (PlayablePSoM psom)))
     tuneResult
  pure unit
