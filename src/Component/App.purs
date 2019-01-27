module Component.App
  ( app
  ) where

import React.Basic (Component, JSX, Self, StateUpdate(..), createComponent, make)
import React.Basic.DOM as H

type Props =
  {}

type State =
  {}

data Action
  = Noop

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  {}

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
            [ H.li_ [ H.text "Emil, Hans" ]
            , H.li_ [ H.text "Mustermann, Max" ]
            , H.li_ [ H.text "Tisch, Roman" ]
            ]
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
