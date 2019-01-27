module Component.App
  ( app
  ) where

import Prelude
import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, createComponent, make)
import React.Basic.DOM as H
import React.Basic.DOM.Events (targetValue)

type Name = { name :: String, surname :: String }

nameToString :: Name -> String
nameToString { name, surname } = name <> ", " <> surname

filterName :: String -> Name -> Boolean
filterName "" _ = true
filterName query { name, surname } =
  let
    pattern = String.Pattern query
    startsWith p s = String.indexOf p s == Just 0
  in
    startsWith pattern name || startsWith pattern surname

filterNames :: String -> Array Name -> Array Name
filterNames query names = Array.filter (filterName query) names

type Props =
  {}

type State =
  { names :: Array Name
  , query :: String
  }

data Action
  = EditQuery String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  { names:
    [ { name: "Emil", surname: "Hans" }
    , { name: "Mustermann", surname: "Max" }
    , { name: "Tisch", surname: "Roman" }
    ]
  , query: ""
  }

render :: Self Props State Action -> JSX
render self =
  H.div
  { className: "app"
  , children:
    [ H.div
      { className: "header"
      , children:
        [ H.h1_
          [ H.text "App" ]
        ]
      }
    , H.div
      { className: "body"
      , children:
        [ H.div_
          [ H.text "Filter prefix:"
          , H.input
            { onChange:
                capture
                  self
                  targetValue
                  (\v -> EditQuery (fromMaybe "" v))
            , value: self.state.query
            }
          ]
        , H.div_
          [ H.ul_
            (map
              (\n -> H.li_ [ H.text n ])
              (map
                nameToString
                (filterNames self.state.query self.state.names)))
          ]
        , H.div_
          [ H.label_
            [ H.span_ [ H.text "Name:" ]
            , H.input {}
            ]
          , H.label_
            [ H.span_ [ H.text "Surname:" ]
            , H.input {}
            ]
          ]
        , H.div_
          [ H.button_ [ H.text "CREATE" ]
          , H.button_ [ H.text "UPDATE" ]
          , H.button_ [ H.text "DELETE" ]
          ]
        ]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self (EditQuery q) =
  Update self.state { query = q }
