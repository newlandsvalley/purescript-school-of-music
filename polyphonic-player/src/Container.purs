module Container where

import Prelude

import Abc.EnsembleScore.Renderer (renderPolyphonicTune)
import Audio.Euterpea.Playable (PlayablePSoM(..))
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Abc (AbcTune)
import Data.Abc.Utils (getTitle)
import Data.Abc.PSoM.Polyphony (generateDSL, generateDSL')
import Data.Abc.Parser (parse) as ABC
import Data.Abc.Voice (getVoiceMap)
import Data.Array (cons, index, null, fromFoldable, mapWithIndex, range)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..)) as Unsafe
import Data.Either (Either(..), either, hush)
import Data.Euterpea.DSL.Parser (PSoM, parse)
import Data.Foldable (foldr)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(..))
import Data.Map (Map, empty, keys, lookup, size, toUnfoldable, values)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.MediaType (MediaType(..))
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, gleitzmanNames, readGleitzman)
import Data.Set (toUnfoldable) as Set
import Data.String (stripPrefix)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.FileInputComponent as FIC
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MultipleSelectComponent as MSC
import Halogen.PlayerComponent as PC
import Partial.Unsafe (unsafePartial)
import StringParser (ParseError)
import Type.Proxy (Proxy(..))
import VexFlow.Score (Renderer, clearCanvas, renderFinalTune, resizeCanvas, initialiseCanvas) as Score
import VexFlow.Types (Config, Titling(..))
import VexFlow.Abc.TickableContext (defaultNoteSeparation)
import Window (print)

type State =
  { instruments :: Array Instrument
  , tuneResult :: Either ParseError AbcTune
  , voicesMap :: Map String AbcTune
  , currentVoice :: Maybe String
  , ePsom  :: Either ParseError PSoM
  , fileName :: Maybe String
  , vexRenderers :: Array Score.Renderer
  }

data Action =
    Init
  | HandleABCFile FIC.Message
  | HandleClear 
  | HandlePrint
  | HandleTuneIsPlaying PC.Message
  | NewInstrumentsSelection MSC.Message
  | HandleChangeVoice String

-- | a simple button has no parameters and is greyed if there's no valid tune
data SimpleButtonType =
    Clear
  | Print  


data Query a =
    HandleNewTuneText a
  | ClearOldTune a

maxVoices :: Int 
maxVoices = 5

allVoices :: String 
allVoices = "all voices"

voiceNamePrefix :: String 
voiceNamePrefix = "voice: "
  
vexConfig :: Int -> Config
vexConfig index =
  { parentElementId : ("vexflow" <> show index)
  , width : 1300
  , height : 10
  , scale : 0.8
  , isSVG : true
  , titling : TitlePlusOrigin
  , noteSeparation: defaultNoteSeparation
  , showChordSymbols : false
  }

abcFileInputCtx :: FIC.Context
abcFileInputCtx =
  { componentId : "abcinput"
  , isBinary : false
  , prompt : "load"
  , accept : mediaType (MediaType ".abc, .txt")
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
  ( abcfile :: FIC.Slot Unit
  , instrument :: MSC.Slot Unit
  , player :: (PC.Slot PlayablePSoM) Unit
  )

_abcfile = Proxy :: Proxy "abcfile"
_instrument = Proxy :: Proxy "instrument"
_player = Proxy :: Proxy "player"

component :: forall i o. H.Component Query i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
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
    , vexRenderers: []
    }

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o Aff Unit
  handleAction = case _ of
    Init -> do
      instruments <- H.liftAff $  loadRemoteSoundFonts  [AcousticGrandPiano, Vibraphone, AcousticBass]
      let
        rows :: Array Int
        rows = range 0 (maxVoices - 1)
      renderers <- H.liftEffect $ traverse (\r -> Score.initialiseCanvas $ vexConfig r) rows
      H.modify_ (\st -> st { vexRenderers = renderers } )
      _ <- H.modify (\st -> st { instruments = instruments
                               , vexRenderers = renderers } )
      pure unit
    HandleABCFile (FIC.FileLoaded filespec) -> do
      let 
        tuneResult = parseTune filespec.contents
      _ <- H.modify (\st -> st { tuneResult = tuneResult
                               , fileName = Just filespec.name } )
      case tuneResult of 
        Right _tune -> do
          _ <- H.tell _player unit PC.StopMelody
          _ <- handleQuery (HandleNewTuneText unit)
          pure unit
        Left _err -> do
          _ <- handleQuery (ClearOldTune unit)
          pure unit
    HandleClear -> do
      _ <- H.modify (\st -> st { fileName = Nothing } )
      _ <- handleQuery (ClearOldTune unit)
      pure unit
    HandlePrint -> do
      _ <-  H.liftEffect print
      pure unit
    NewInstrumentsSelection (MSC.CommittedSelections pendingInstrumentNames) -> do
      let
        f s acc =
          case readGleitzman s of
            Just inst -> cons inst acc
            _ -> acc
        instrumentNames :: Array InstrumentName
        instrumentNames = foldr f [] pendingInstrumentNames
      instruments <- H.liftAff $ loadRemoteSoundFonts instrumentNames
      _ <- H.tell _player unit $ (PC.SetInstruments instruments)
      _ <- H.modify (\st -> st { instruments = instruments})
      pure unit
    HandleTuneIsPlaying (PC.IsPlaying _) -> do
      -- we ignore this message, but if we wanted to we could
      -- disable any button that can alter the editor contents whilst the player
      -- is playing and re-enable when it stops playing
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
        currentVoice = 
          if ( voice == allVoices ) then Just allVoices
          else 
            stripPrefix (Pattern voiceNamePrefix) voice
      reloadPlayer (state { currentVoice = currentVoice } )
      _ <- H.modify (\st -> st { currentVoice = currentVoice })
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o Aff (Maybe a)
  handleQuery = case _ of    
    HandleNewTuneText next -> do
      state <- H.get
      case state.tuneResult of
        Right tune -> do
          let 
            voicesMap = getVoiceMap tune
            currentVoice = Just allVoices
            ePsom = generatePsom state currentVoice tune  
          _ <- displayRenderedScores state voicesMap tune
          _ <- refreshPlayerState ePsom
          _ <- H.modify (\st -> st { voicesMap = voicesMap
                                   , currentVoice = currentVoice
                                   , ePsom = ePsom
                                   } )
          pure (Just next)
        Left _ -> 
          pure (Just next)
    ClearOldTune next -> do
      state <- H.get
      _ <- H.tell _player unit $ PC.StopMelody
      _ <- H.modify (\st -> st { tuneResult = nullAbcTune
                               , voicesMap = empty :: Map String AbcTune
                               , currentVoice = Nothing
                               , ePsom = nullPsomTune
                               } )
      _ <- H.liftAff $ clearScores state
      pure (Just next)


  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render state = HH.div_
    [ HH.h1
      [HP.class_ (H.ClassName "center") ]
      [HH.text "Polyphonic ABC Player"]  

      -- left pane - instruments
    , HH.div
        [ HP.class_ (H.ClassName "leftPane") ]
        [ 
          -- load instruments
          HH.div
            [ HP.class_ (H.ClassName "panelComponent")]
            [ HH.h2 
                []
                [HH.text "Instruments"]  
            , HH.slot _instrument unit
               (MSC.component multipleSelectCtx initialMultipleSelectState) unit NewInstrumentsSelection
            ]
          -- display instruments
          , renderInstruments state
          -- player
          , renderPlayer state
        ]
        
    , HH.div
      -- right pane - ABC
      [ HP.class_ (H.ClassName "rightPane") ]
      [
        -- load and clear
        HH.div
         [ HP.class_ (H.ClassName "panelComponent") ]
         [ HH.h2 
                []
                [HH.text "Tune"]  
         , HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "ABC:" ]
         , HH.slot _abcfile unit (FIC.component abcFileInputCtx) unit HandleABCFile
         , renderSimpleButton Clear state
         ]
         -- print
      , HH.div
         [ HP.class_ (H.ClassName "panelComponent") ]
         [  HH.label
            [ HP.class_ (H.ClassName "labelAlignment") ]
            [ HH.text "score:" ]
         , renderSimpleButton Print state
         ]
        -- render voice menu if we have more than 1 voice
      , renderPossibleVoiceMenu state
      ]   
      
      -- score rendering
    , possiblyRenderTuneTitle state
    , HH.ul [ HP.id "score"] renderScores   
    , renderParseError state
    --, renderDebug state
    ]

  renderPlayer :: State -> H.ComponentHTML Action ChildSlots Aff
  renderPlayer state =
    case state.ePsom of
      Right psom ->
        HH.div
          [ HP.class_ (H.ClassName "panelComponent")]
          [
             HH.slot _player unit (PC.component (PlayablePSoM psom) state.instruments) unit (HandleTuneIsPlaying)
          ]
      Left _ ->
        HH.div_
          [  ]

  renderInstruments :: State -> H.ComponentHTML Action ChildSlots Aff
  renderInstruments state =
    if (null state.instruments) then
      HH.div
        [ HP.class_ (H.ClassName "panelComponent") ]
        [ HH.text "wait for instruments to load"]
    else
      HH.div
        [ HP.class_ (H.ClassName "panelComponent") ]
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
        voiceNames = cons allVoices (Set.toUnfoldable (keys state.voicesMap)) 
        currentVoice = fromMaybe allVoices state.currentVoice
      in
        renderVoiceMenu currentVoice voiceNames

  renderVoiceMenu :: String -> Array String ->  H.ComponentHTML Action ChildSlots Aff
  renderVoiceMenu currentVoice voices =   
    HH.div
      [ HP.class_ (H.ClassName "panelComponent")]
      [ 
        
        {- HH.label
         [ HP.class_ (H.ClassName "labelAlignment") ]
         [ HH.text "preferences:" ]
      , -}
        HH.div 
         [ HP.class_ (H.ClassName "voiceMenu")] 
         $ mapWithIndex (addVoiceRadio currentVoice) voices
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
        [ HH.text ("play " <> label) ]
      ]
    where 
      isChecked = selected == voiceName
      label =  
        if (voiceName == allVoices) then
          allVoices
        else 
          (voiceNamePrefix <> voiceName)
      voiceId = "voice" <> show id


  {-}
  renderScores :: ∀ m
    . MonadAff m
    => Array (H.ComponentHTML Action ChildSlots m)
  -}
  renderScores :: Array (H.ComponentHTML Action ChildSlots Aff)
  renderScores =
    map renderScoreItem (range 0 (maxVoices -1))

  --renderScoreItem :: ∀ i p. State -> Int -> HH.HTML i p
  renderScoreItem idx =
    HH.li
      [ HP.class_ (H.ClassName "scoreItem") ]
      [ HH.div
        [ HP.id ("vexflow" <> show idx)
        , HP.class_ (H.ClassName "canvasDiv")
        ]
        []
      ]

  -- render the overall tune title in those cases where we have more than 
  -- one voice which will be titled separately in each score
  possiblyRenderTuneTitle :: State -> H.ComponentHTML Action ChildSlots Aff
  possiblyRenderTuneTitle state =
    if (size state.voicesMap <= 1) then 
      HH.text ""
    else
      case (hush state.tuneResult >>= getTitle) of
        Just title ->
          HH.h2
            [HP.id "tune-title" ]
            [HH.text title ]
        _ ->
          HH.text ""
  
  renderParseError :: 
       State
    -> H.ComponentHTML Action ChildSlots Aff
  renderParseError state =
    case (hush state.tuneResult) of 
      Just _tune -> 
        HH.text ""
      _ ->
        case state.fileName of 
          Just _name -> 
            HH.text ("ABC file failed to parse") 
          _ ->
            HH.text ""
  
  -- rendering functions
  renderSimpleButton :: 
       SimpleButtonType
    -> State
    -> H.ComponentHTML Action ChildSlots Aff
  renderSimpleButton buttonType state =
    let
      label = case buttonType of
        Clear -> "clear"
        Print -> "print"
      action = case buttonType of
        Clear -> HandleClear
        Print -> HandlePrint
      enabled =
        either (\_ -> false) (\_ -> true) state.tuneResult
      className =
          either (\_ -> "unhoverable") (\_ -> "hoverable") state.tuneResult
    in
      HH.button
        [ HE.onClick \_ -> action
        , HP.class_ $ ClassName className
        , HP.enabled enabled
        ]
        [ HH.text label ]

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

parseTune :: String -> Either ParseError AbcTune
parseTune text = 
  -- we need to add a terminating bar line
  ABC.parse (text <> "|\r\n")

-- Generate the possibly polyphonic PSoM DSL
-- if the user selects just one voice or there is only once voice in the tune anyway 
-- then this defaults to a monophonic tune DSL
generatePsom :: State -> Maybe String -> AbcTune -> Either ParseError PSoM
generatePsom state mCurrentVoice tune =     
  let
    instrumentNames = map fst state.instruments
    voicesMap = getVoiceMap tune
    dsl = 
      -- polyphony or only one voice anyway
      if mCurrentVoice == Nothing || mCurrentVoice == Just allVoices || (size state.voicesMap <= 1) then
        let
          -- the user has by now selected a voice so the coice map cannot be empty
          voices = Unsafe.NonEmptyArray $ fromFoldable (values voicesMap)
          title = fromMaybe "unnamed" $ getTitle tune 
        in
          generateDSL' voices instrumentNames title
      -- monophony - choose one out of a set of voices
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

displayRenderedScores :: ∀ o.
       State
    -> Map String AbcTune 
    -> AbcTune
    -> H.HalogenM State Action ChildSlots o Aff Unit
displayRenderedScores state voicesMap tune = do
  let 
     -- single renderer is used unless we have multiple voices that 
     -- cannot be rendered in an ensemble score (maybe because parts don't match)
     singleRenderer = unsafePartial $ fromJust $ index state.vexRenderers 0
    -- render the whole tune if only one voice
  if (size voicesMap) <= 1 then do
    _ <- H.liftAff $ clearScores state
    _ <- H.liftEffect $ Score.renderFinalTune (vexConfig 0) singleRenderer tune
    pure unit
  -- otherwise render the selected voice
  else do 
    -- first try to render the ensemble score
    let 
      ensembleConfig = (vexConfig 0) { scale = 0.6}
    _ <- H.liftAff $ clearScores state
    mError <- H.liftEffect $ renderPolyphonicTune ensembleConfig singleRenderer tune
    -- but fall back to displaying the voices individually
    when (isJust mError) do
      let 
        error = fromMaybe "none"  mError         
        -- displayVoice :: Int -> Tuple String AbcTune -> H.HalogenM State Action ChildSlots o Aff Unit
        displayVoice idx voices =
          case (index state.vexRenderers idx) of 
            Just renderer -> do
              _ <- H.liftEffect $ Score.clearCanvas $ renderer
              _ <- H.liftEffect $ Score.renderFinalTune (vexConfig idx) renderer (snd voices)
              pure unit
            _ -> pure unit
        voiceNamesAndTunes :: Array (Tuple String AbcTune)
        voiceNamesAndTunes = toUnfoldable voicesMap
      
      _ <- H.liftEffect $ log ("ensemble score error: " <> error)
      _ <- H.liftAff $ clearScores state
      H.liftEffect $ traverseWithIndex_ displayVoice voiceNamesAndTunes


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
     (\_ -> H.tell _player unit PC.StopMelody)
     (\psom -> H.tell _player unit (PC.HandleNewPlayable (PlayablePSoM psom)))
     tuneResult
  pure unit

clearScores :: State -> Aff Unit
clearScores state = do
  let
    f :: Int -> Score.Renderer -> Effect Score.Renderer
    f i renderer = Score.resizeCanvas renderer (vexConfig i)
  _ <- H.liftEffect $ traverseWithIndex_ f state.vexRenderers
  _ <- H.liftEffect $ traverse (Score.clearCanvas) state.vexRenderers      
  pure unit
