module Halogen.PlayerComponent where

import Prelude

import Audio.SoundFont (AUDIO, Instrument, playNotes, instrumentChannels)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.State.Class (class MonadState)
import Data.Array (null, index, length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (PropName(..))
import Halogen.PlayerComponent.Style (capsuleStyle, playerBlockStyle, playerStyle
  , buttonStyle)
import Audio.SoundFont.Melody.Class (class Playable, toMelody)
import Audio.SoundFont.Melody (Melody, MidiPhrase)


-- | now we have tri-state logic for playback state because of the pending status
data PlaybackState =
    PLAYING           -- the melody is playing
  | PENDINGPAUSED     -- the pause button has been hit, but the phrase is stil finishing
  | PAUSED            -- the melody is not playing  (paused or stopped)

derive instance genericPlaybackState :: Generic PlaybackState _
instance showEvent :: Show PlaybackState where
  show = genericShow
instance eqEvent :: Eq PlaybackState where
  eq = genericEq

data Query a =
    SetInstruments (Array Instrument) a
  | PlayMelody PlaybackState a             -- play | pause
  | StepMelody a                           -- step to the next phrase
  | StopMelody a                           -- stop and set index to zero
  | EnablePlayButton a                     -- re-enable the play button


type State =
  { instruments :: Array Instrument  -- the instrument soundfonts available
  , melody :: Melody                 -- the melody to play
  , playing :: PlaybackState         -- the state of the playback
  , phraseIndex :: Int               -- the current phrase being played
  , phraseLength :: Number           -- the duration of the phrase currently playing
  }

component :: ∀ eff p. Playable p => p -> Array Instrument -> H.Component HH.HTML Query Unit Void (Aff (au :: AUDIO | eff))
component playable instruments =
  H.component
    { initialState: const (initialState instruments)
    , render
    , eval: eval playable
    , receiver: const Nothing
    }
  where

  -- | the initial state of the player (with no melody to play yet)
  initialState :: Array Instrument -> State
  initialState instruments =
    { instruments : instruments
    , melody : []
    , playing : PAUSED
    , phraseIndex : 0
    , phraseLength : 0.0
    }


  render :: State -> H.ComponentHTML Query
  render state =
    let
      sliderPos =
        toNumber $ state.phraseIndex
      capsuleMax =
        toNumber $ length state.melody
      startImg =
        "assets/images/play.png"
      stopImg =
        "assets/images/stop.png"
      pauseImg =
        "assets/images/pause.png"
      -- the action toggles the PLAYING - PAUSED status
      playAction =
        if (state.playing == PLAYING) then
           PlayMelody PAUSED
        else
           PlayMelody PLAYING
      playButtonImg =
        if (state.playing == PAUSED) then
          startImg
        else
          -- the pause image is displayed both if the tune is to be PlayMelody
          -- or else if it is disabled pending a pause
          pauseImg
      -- the buttons are temporarily disabled after a pause command
      isDisabled =
        (state.playing == PENDINGPAUSED)
    in
      HH.div [ playerBlockStyle ]
        [ HH.div [ playerStyle ]
          [

                 -- , playerStyle
            HH.input
              [ HP.type_ HP.InputImage
              , HP.disabled isDisabled
              , HP.src playButtonImg
              , HE.onClick (HE.input_ playAction)
              , buttonStyle
              ]
          ,  HH.input
              [ HP.type_ HP.InputImage
              , HP.disabled isDisabled
              , HP.src stopImg
              , HE.onClick (HE.input_ StopMelody)
              , buttonStyle
              ]
          , HH.progress
                [ HP.max capsuleMax
                , progressValue sliderPos
                , capsuleStyle
                ] []
          ]
          {- debug
          , HH.div_
            [ HH.text ("melody length: " <> show (length state.melody))
            , HH.text ("no of instruments: " <> show (length state.instruments))]
          -}
        ]

  eval :: ∀ eff p. Playable p => p -> Query ~> H.ComponentDSL State Query Void (Aff (au :: AUDIO | eff))
  eval p = case _ of

    -- when we change the instruments (possibly im mid-melody) we need to
    -- re-initialise and remove the old melody which will need to be
    -- recomputed with the new instruments (done when play is first pressed) 
    SetInstruments instruments next -> do
      H.modify (\state -> state { instruments = instruments
                                , phraseIndex = 0
                                , playing = PENDINGPAUSED
                                , melody = []
                                })
      pure next


    -- PlayMelody responds to the toggled button PLAY/PAUSE
    -- however it also establishes the melody on first reference
    PlayMelody button next -> do
      state <- H.get

      when (null state.melody) do
        establishMelody p

      state <- H.get

      if ((button == PLAYING) && (not (null state.melody)))
        then do
          -- play
          H.modify (\state -> state { playing = PLAYING})
          eval p (StepMelody next)
        else do
          -- pause
          nextInstruction <- temporarilyFreezePlayButton
          eval p (nextInstruction next)

    -- StepMelody plays the current phrase and then steps the pointer to the next one
    -- it must respect any button presses in between steps
    StepMelody next -> do
      state <- H.get

      if ((state.playing == PLAYING) && (not (null state.melody)))
        then do
          -- play
          nextInstruction <- step
          eval p (nextInstruction next)
        else do
          -- pause
          nextInstruction <- temporarilyFreezePlayButton
          eval p (nextInstruction next)

    -- EnablePlayButton unfreezes the play button which is frozen after being
    -- pressed so as to avoid playing the melody twice simultaneously
    EnablePlayButton next -> do
      H.modify (\state -> state { playing = PAUSED})
      pure next

    -- StopMelody resets the melody index back to the start
    StopMelody next -> do
      state <- H.get
      if (state.playing == PLAYING)
        then do
          _ <- temporarilyFreezePlayButton
          state <- H.get
          H.modify (\state -> state { phraseIndex = 0, playing = PAUSED})
          pure next
        else do
          H.modify (\state -> state { phraseIndex = 0, playing = PAUSED})
          pure next


establishMelody :: ∀ m eff p.
  Bind m =>
  MonadState State m =>
  MonadAff eff m =>
  Playable p =>
  p ->
  m Unit
establishMelody playable = do
  state <- H.get
  let
    melody = toMelody playable (instrumentChannels state.instruments)
  H.modify (\state -> state { melody = melody})
  pure unit


step :: forall m eff t64 a.
    Bind m =>
    MonadState State m =>
    MonadEff ( au :: AUDIO | t64) m =>
    MonadAff eff m =>
    m ( a -> Query a)
step = do
  state <- H.get
  let
    mPhrase = locateNextPhrase state
  case mPhrase of
    Just (midiPhrase) -> do
      -- play the phrase
      phraseLength <- H.liftEff (playEvent state.instruments midiPhrase)
      -- step the index and put it into the state
      let
        newState =
          state { phraseIndex = state.phraseIndex + 1
                , phraseLength = phraseLength
                }
      H.put newState
      -- delay whilst we're playing the phrase
      _ <-  H.liftAff $ delay (Milliseconds ((phraseLength) * 1000.0))
      pure StepMelody
    _ ->
      pure StopMelody


temporarilyFreezePlayButton :: ∀ m eff a.
  Bind m =>
  MonadState State m =>
  MonadAff eff m =>
  m (a -> Query a)
temporarilyFreezePlayButton = do
  state <- H.get
  H.put $ state { playing = PENDINGPAUSED }
  let
    msDelay = state.phraseLength
  _ <-  H.liftAff $ delay (Milliseconds (msDelay * 1000.0))
  pure EnablePlayButton

-- | locate the next MIDI phrase from the performance
locateNextPhrase :: State -> Maybe MidiPhrase
locateNextPhrase state =
  if (not (state.playing == PLAYING)) || (null state.melody) then
    Nothing
  else
    index state.melody (state.phraseIndex)

-- | play a MIDI Phrase (a bunch of MIDI notes)
-- | only NoteOn events produce sound
playEvent :: ∀ eff. Array Instrument -> MidiPhrase -> Eff (au :: AUDIO | eff) Number
playEvent instruments midiPhrase =
  playNotes instruments midiPhrase

-- halogen bug workaround
progressValue :: forall r i. Number -> HP.IProp (value :: Number | r) i
progressValue = HP.prop (PropName "value")
