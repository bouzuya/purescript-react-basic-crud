module Component.App
  ( app
  ) where

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Prelude (map, (<>), (==), (||))
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, capture_, createComponent, make)
import React.Basic.DOM as H
import React.Basic.DOM.Events (targetValue)

type Name = { name :: String, surname :: String }

emptyName :: Name
emptyName = { name: "", surname: "" }

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

nameToString :: Name -> String
nameToString { name, surname } = name <> ", " <> surname

type Props =
  {}

type State =
  { edited :: Name
  , names :: Array Name
  , query :: String
  , selected :: Maybe Name
  }

data Action
  = EditName String
  | EditQuery String
  | EditSurname String
  | SelectName Name

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  { edited: emptyName
  , names:
    [ { name: "Emil", surname: "Hans" }
    , { name: "Mustermann", surname: "Max" }
    , { name: "Tisch", surname: "Roman" }
    ]
  , query: ""
  , selected: Nothing
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
          [ H.style_
            [ H.text
              ".is-selected { background-color: #0000ff; color: #ffffff; }"
            ]
          , H.ul_
            (map
              (\name ->
                H.li
                { className:
                    if self.state.selected == Just name
                    then "is-selected"
                    else ""
                , children:
                  [ H.text (nameToString name) ]
                , onClick: capture_ self (SelectName name)
                })
              (filterNames self.state.query self.state.names))
          ]
        , H.div_
          [ H.label_
            [ H.span_ [ H.text "Name:" ]
            , H.input
              { onChange:
                  capture
                    self
                    targetValue
                    (\v -> EditName (fromMaybe "" v))
              , value: self.state.edited.name
              }
            ]
          , H.label_
            [ H.span_ [ H.text "Surname:" ]
            , H.input
              { onChange:
                  capture
                    self
                    targetValue
                    (\v -> EditSurname (fromMaybe "" v))
              , value: self.state.edited.surname
              }
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
update self (EditName s) =
  Update self.state { edited = self.state.edited { name = s } }
update self (EditQuery s) =
  Update self.state { query = s }
update self (EditSurname s) =
  Update self.state { edited = self.state.edited { surname = s } }
update self (SelectName n) =
  Update
    self.state
    { edited = n
    , selected = Just n
    }
