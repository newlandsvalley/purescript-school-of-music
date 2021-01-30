module Container where

import Prelude

import Audio.Euterpea.Playable (PlayablePSoM(..))
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Abc (AbcTune)
import Data.Abc.Metadata (getTitle)
import Data.Abc.PSoM.Polyphony (generateDSL')
import Data.Abc.Parser (PositionedParseError(..)) as ABC
import Data.Abc.Voice (getVoiceMap)
import Data.Array (cons, head, null, fromFoldable)
import Data.Either (Either(..), either, hush)
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError(..), parse)
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Map (Map, empty, keys, lookup, size, values)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, gleitzmanNames, readGleitzman)
import Data.Set (toUnfoldable) as Set
import Data.String (stripPrefix)
import Data.String.Pattern (Pattern(..))
import Data.Symbol (SProxy(..))
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
import VexFlow.Abc.Alignment (rightJustify)
import VexFlow.Score (Renderer, clearCanvas, createScore, renderScore, initialiseCanvas) as Score
import VexFlow.Types (Config, VexScore)

import Debug.Trace (spy)

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either ABC.PositionedParseError AbcTune
  , voicesMap :: Map String AbcTune
  , currentVoice :: Maybe String
  , ePsom  :: Either PositionedParseError PSoM
  , fileName :: Maybe String
  , vexRenderer :: Maybe Score.Renderer
  , vexScore :: VexScore
  }

data Action =
    Init
  | HandleABCFile FIC.Message
  | HandleClearButton Button.Message
  | HandleNewTuneText ED.Message
  | HandleTuneIsPlaying PC.Message
  | NewInstrumentsSelection MSC.Message
  | HandleChangeVoice String

voiceNamePrefix :: String 
voiceNamePrefix = "voice: "


vexConfig :: Config
vexConfig =
  { parentElementId : "vexflow"
  , width : 1300
  , height : 700
  , scale : 0.8
  , isSVG : true
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
nullAbcTune :: Either ABC.PositionedParseError AbcTune
nullAbcTune =
  Left (ABC.PositionedParseError { pos : 0, error : "" })

nullPsomTune :: Either PositionedParseError PSoM
nullPsomTune =
  Left (PositionedParseError { pos : 0, error : "" })

parseError :: Either ABC.PositionedParseError AbcTune -> String
parseError tuneResult =
  case tuneResult of
    Right _ -> "no errors"
    Left (ABC.PositionedParseError ppe) -> "parse error: " <> ppe.error

type ChildSlots =
  ( editor :: ED.Slot Unit
  , abcfile :: FIC.Slot Unit
  , instrument :: MSC.Slot Unit
  , clear :: Button.Slot Unit
  , player :: (PC.Slot PlayablePSoM) Unit
  )

_editor = SProxy :: SProxy "editor"
_abcfile = SProxy :: SProxy "abcfile"
_instrument = SProxy :: SProxy "instrument"
_clear = SProxy :: SProxy "clear"
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
    , tuneResult: nullAbcTune
    , voicesMap : empty
    , currentVoice : Nothing
    , ePsom: nullPsomTune
    , fileName: Nothing
    , vexRenderer: Nothing
    , vexScore: Left ""
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
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent filespec.contents)
      _ <- H.query _player unit $ H.tell PC.StopMelody
      pure unit
    HandleClearButton (Button.Toggled _) -> do
      _ <- H.modify (\st -> st { fileName = Nothing
                               } )
      _ <- H.query _editor unit $ H.tell (ED.UpdateContent "")
      pure unit
    HandleNewTuneText (ED.TuneResult eTuneResult) -> 
      case eTuneResult of
        Right tune -> do
          state <- H.get
          let 
            voicesMap = getVoiceMap tune
            ePsom = generatePsom state.instruments tune (fromFoldable (values voicesMap))
            voiceNames = getVoiceNames voicesMap
            -- render the score with no RHS alignment
            currentVoice = head voiceNames
            vexScore = generateScore currentVoice voicesMap tune
          _ <- displayScore state.vexRenderer vexScore
          _ <- refreshPlayerState ePsom
          _ <- H.modify (\st -> st { tuneResult = eTuneResult
                                   , voicesMap = voicesMap
                                   , currentVoice = currentVoice
                                   , ePsom = ePsom
                                   , vexScore = vexScore} )
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
    HandleChangeVoice voice -> do          
      state <- H.get
      -- strip the 'voice: ' prefix from the voice name we get from the menu
      let 
        currentVoice = stripPrefix (Pattern voiceNamePrefix) voice
        vexScore = 
          case state.tuneResult of 
            Right tune ->
              generateScore currentVoice state.voicesMap tune
            Left _ ->
              Left "nothing"
      _ <- displayScore state.vexRenderer vexScore
      _ <- H.modify (\st -> st { currentVoice = currentVoice
                               , vexScore = vexScore })
      pure unit


  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render state = HH.div_
    [ HH.h1
      [HP.class_ (H.ClassName "center") ]
      [HH.text "PureScript Polyphonic ABC Player"]
    , HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        -- load
        HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent") ]
         [  HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "load ABC:" ]
         , HH.slot _abcfile unit (FIC.component abcFileInputCtx) unit (Just <<< HandleABCFile)
         ]
      , HH.div
          -- clear
          [ HP.class_ (H.ClassName "leftPanelComponent")]
          [ HH.label
             [ HP.class_ (H.ClassName "labelAlignment") ]
             [ HH.text "clear:" ]
          -- clear
          , HH.slot _clear unit (Button.component "clear") unit (Just <<< HandleClearButton)
          ]
        -- render a voice menu if we have more than 1 voice
      , renderPossibleVoiceMenu state
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
             HH.slot _player unit (PC.component (PlayablePSoM psom) state.instruments) unit (Just <<< HandleTuneIsPlaying)
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

  -- we only render this menu if we have more than 1 voice
  renderPossibleVoiceMenu :: State -> H.ComponentHTML Action ChildSlots Aff 
  renderPossibleVoiceMenu state = 
    if (size state.voicesMap <= 1) then
      HH.div_
        []
    else
      let 
        voiceNames = Set.toUnfoldable (keys state.voicesMap)
        currentVoice = fromMaybe "none" state.currentVoice
      in
        renderVoiceMenu currentVoice voiceNames

  renderVoiceMenu :: String -> Array String ->  H.ComponentHTML Action ChildSlots Aff
  renderVoiceMenu currentVoice voices =
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ HH.label
         [ HP.class_ (H.ClassName "labelAlignment") ]
         [ HH.text "choose voice: " ]
      , HH.select
          [ HP.class_ $ H.ClassName "selection"
          , HP.id_  "voice-menu"
          , HP.value (voiceNamePrefix <> currentVoice)
          , HE.onValueChange
              (Just <<<  HandleChangeVoice)
          ]
          (voiceOptions voices currentVoice)
      ]

  voiceOptions :: ∀ a b. Array String -> String -> Array (HH.HTML a b)
  voiceOptions voices currentVoice =
    let
      f :: ∀ p ix. String -> HH.HTML p ix
      f voice =
        let
           disabled = (voice == currentVoice)
        in
          HH.option
            [ HP.disabled disabled ]
            [ HH.text (voiceNamePrefix <> voice)]
    in
      map f voices 

  {-}
  renderScore :: ∀ m
    . MonadAff m
    => State
    -> H.ComponentHTML Action ChildSlots m
  -}
  renderScore :: State -> H.ComponentHTML Action ChildSlots Aff
  renderScore state =
    HH.div
      [ HP.id_ "score"]
      [ renderTuneTitle state
        , HH.div
           [ HP.class_ (H.ClassName "canvasDiv")
           , HP.id_ "vexflow"
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
            [HP.id_ "tune-title" ]
            [HH.text (title <> voiceName)]
        _ ->
          HH.text ""
     

  
  renderDebug :: State -> H.ComponentHTML Action ChildSlots Aff 
  renderDebug state = 
    HH.div_
      [ HH.text ("current voice:" <> (fromMaybe "none" state.currentVoice)) ]

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
 
generatePsom :: Array Instrument -> AbcTune -> Array AbcTune -> Either PositionedParseError PSoM
generatePsom instruments tune voices =     
  let
    instrumentNames = map fst instruments
    title = fromMaybe "unnamed" $ getTitle tune 
    dsl = generateDSL' voices instrumentNames title
  in    
    parse dsl

getVoiceNames :: Map String AbcTune -> Array String 
getVoiceNames voicesMap = 
  fromFoldable (keys voicesMap)       

generateScore :: Maybe String -> Map String AbcTune -> AbcTune -> VexScore
generateScore mCurrentVoice voicesMap tune = 
  if (size voicesMap) <= 1 then
    Score.createScore vexConfig tune
  else 
    let 
      currentVoice = fromMaybe "nothing" mCurrentVoice
      _ = spy "current voice" currentVoice
      voiceTune = fromMaybe tune $ lookup currentVoice voicesMap 
    in
      Score.createScore vexConfig voiceTune


displayScore :: ∀ o.
       Maybe Score.Renderer
    -> VexScore
    -> H.HalogenM State Action ChildSlots o Aff Unit
displayScore mRenderer vexScore = 
  case mRenderer of 
    Nothing -> 
      pure unit
    Just renderer -> do
      let 
        justifiedScore = rightJustify vexConfig.width vexConfig.scale vexScore
      _ <- H.liftEffect $ Score.clearCanvas $ renderer
      rendered <- H.liftEffect $ Score.renderScore vexConfig renderer justifiedScore
      pure unit
  

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
