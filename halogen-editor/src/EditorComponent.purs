module EditorComponent where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Euterpea.DSL.Parser (PSoM, PositionedParseError, parse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = String

data Query a =
    UpdateContent String a
  | GetState (String -> a)

data Message = TuneResult (Either PositionedParseError PSoM)

component :: forall m. String -> H.Component HH.HTML Query Unit Message m
component label =
  H.component
    { initialState: const initialState
    , render: render label
    , eval
    , receiver: const Nothing
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
        , HE.onValueInput (HE.input UpdateContent)
        ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    UpdateContent s next -> do
      H.modify (\state -> s )
      H.raise $ TuneResult $ parse s
      pure next
    GetState reply -> do
      state <- H.get
      pure (reply state)
