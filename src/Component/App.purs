module Component.App
  ( app
  ) where

import Prelude

import React.Basic (Component, JSX, Self, StateUpdate(..), createComponent, make)
import React.Basic.DOM as H

type Name = { name :: String, surname :: String }

nameToString :: Name -> String
nameToString { name, surname } = name <> ", " <> surname

type Props =
  {}

type State =
  { names :: Array Name }

data Action
  = Noop

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
          , H.input {} ]
        , H.div_
          [ H.ul_
            (map (\n -> H.li_ [ H.text n ]) (map nameToString self.state.names))
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
update self Noop = NoUpdate
