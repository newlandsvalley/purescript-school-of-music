module Halogen.MultipleSelectComponent where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List(..), elem, filter, reverse, toUnfoldable, (:))
import Data.Array (cons) as A
import Control.Monad.Aff (Aff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..), HTML)
import Halogen.MultipleSelectComponent.Dom (SDOM, resetDefaultSelected)

data Query a =
    AddSelection String a
  | RemoveSelection String a
  | ClearSelections a
  | CommitSelections a
  | GetSelections (List String -> a)

data Message = CommittedSelections (List String)

type Context = {
    commitPrompt       :: String   -- the user prompt for committing changes
  , commitButtonText   :: String   -- the text on the commit button
  }

type State = {
    instruction :: String          -- the instruction on what to select
  , available :: List String       -- available options
  , selected  :: List String       -- currently selected options
  }

component :: ∀ eff. Context -> State -> H.Component HH.HTML Query Unit Message (Aff (sdom :: SDOM | eff))
component ctx initialState =
  H.component
    { initialState: const $ initialState
    , render: render ctx
    , eval
    , receiver: const Nothing
    }
  where

  render :: Context -> State -> H.ComponentHTML Query
  render ctx state =
    HH.div
      [ HP.class_ $ ClassName "msSelectDiv" ]
      [ addSelectionDropdown state
      , viewSelections state
      , commitSelectionsButton ctx state
      ]

  -- allow the user to add a selection to the growing multi-select list
  addSelectionDropdown :: State -> H.ComponentHTML Query
  addSelectionDropdown state =
    let
      f :: ∀ p i. String -> HTML p i
      f s =
          HH.option
            [ HP.disabled (elem s state.selected) ]
            [ HH.text s]
    in
      HH.div
        [ HP.class_ $ ClassName "msAddSelectionDiv" ]
        [
          HH.select
            [ HP.class_ $ ClassName "msAddSelection"
            , HP.id_  "selection-menu"
            , HP.value state.instruction
            , HE.onValueChange  (HE.input AddSelection)
            ]
            (A.cons
              (HH.option [ HP.disabled true ] [ HH.text state.instruction])
              (map f $ toUnfoldable state.available)
            )
        ]

  commitSelectionsButton :: Context -> State -> H.ComponentHTML Query
  commitSelectionsButton ctx state =
    case state.selected of
      Nil ->
        HH.div_ []
      _ ->
        HH.div
          [ HP.class_ (H.ClassName "msCommitDiv") ]
          [ HH.label
             [ HP.class_ (H.ClassName "msCommitLabel") ]
             [ HH.text ctx.commitPrompt ]
          , HH.button
             [ HP.class_ $ ClassName "msCommit hoverable"
             , HE.onClick (HE.input_ CommitSelections) ]
             [ HH.text ctx.commitButtonText ]
          ]

  -- list the currently selected options
  viewSelections :: State -> H.ComponentHTML Query
  viewSelections state =
    let
      -- f :: ∀ p i. String -> HTML p i
      f s =
        HH.li
          [ HP.class_ $ ClassName "msListItem" ]
          [ HH.span
              [ HP.class_ $ ClassName  "msListItemLabel" ]
              [ HH.text s]
          , HH.a
              [ HP.class_ $ ClassName  "msListItemRemove"
              , HE.onClick (HE.input_ (RemoveSelection s))
              ]
              [ HH.text " remove"]
          ]
    in
      HH.div_
        (map f $ toUnfoldable state.selected)


  eval :: ∀ eff. Query ~> H.ComponentDSL State Query Message (Aff (sdom :: SDOM | eff))
  eval = case _ of
    AddSelection s next -> do
      H.modify (\state -> state { selected = addSelection s state.selected })
      _ <- H.liftEff resetDefaultSelected
      state <- H.get
      -- H.raise $ CurrentSelections state.selected
      pure next
    RemoveSelection s next -> do
      H.modify (\state -> state { selected = removeSelection s state.selected })
      state <- H.get
      -- H.raise $ CurrentSelections state.selected
      pure next
    ClearSelections next -> do
      H.modify (\state -> state { selected = Nil })
      -- H.raise $ CurrentSelections Nil
      pure next
    CommitSelections next -> do
      state <- H.get
      let
        selected = state.selected
      H.modify (\state -> state { selected = Nil })
      H.raise $ CommittedSelections selected
      pure next
    GetSelections reply -> do
      state <- H.get
      pure (reply state.selected)


-- add a selection to the end of the list
addSelection :: String -> List String -> List String
addSelection s ss =
  reverse $ s : (reverse ss)

-- remove a selection from the list
removeSelection :: String -> List String -> List String
removeSelection s ss =
  filter ((/=) s) ss
