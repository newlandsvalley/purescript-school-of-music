module TextAreaComponent where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = String
type Input = String

data Query a =
    HandleInput String a
  | SetOutput String a

data Message = Contents String

component :: forall m. String -> H.Component HH.HTML Query Input Message m
component label =
  H.component
    { initialState: const initialState
    , render: render label
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: State
  initialState = ""

  render :: String -> State -> H.ComponentHTML Query
  render label state =
      HH.textarea
        [ HP.rows 15
        , HP.cols 70
        , HP.autofocus true
        , HP.value state
        -- , HP.wrap false
        , HE.onValueChange (HE.input SetOutput)
        ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    HandleInput input next -> do
      oldS <- H.get
      when (oldS /= input) $ H.put input
      pure next
    SetOutput s next -> do
      state <- H.get
      H.raise $ Contents state
      pure next
