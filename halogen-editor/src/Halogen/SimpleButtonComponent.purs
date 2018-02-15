module Halogen.SimpleButtonComponent where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))

type State = Boolean

data Query a
  = Toggle a

data Message = Toggled Boolean

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
  initialState = false

  render :: String -> State -> H.ComponentHTML Query
  render label state =
      HH.button
        [ HE.onClick (HE.input_ Toggle)
        , HP.class_ $ ClassName "hoverable"
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
