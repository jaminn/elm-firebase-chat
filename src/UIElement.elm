module UIElement exposing (..)

import Css exposing (..)
import Css.Transitions as Transition exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


toggle msg isSelected =
    let
        widthVal =
            60

        heightVal =
            20

        backgroundColorConditional =
            if isSelected then
                backgroundColor (rgb 0 113 210)

            else
                backgroundColor (rgb 202 202 202)

        translateXConditional =
            if isSelected then
                translateX (px (widthVal - heightVal))

            else
                translateX (px 0)
    in
    div
        [ onClick msg
        , css
            [ width (px widthVal)
            , height (px heightVal)
            , backgroundColorConditional
            , transition [ Transition.backgroundColor 300 ]
            , borderRadius (px 10)
            , position relative
            , property "user-select" "none"
            ]
        ]
        [ div
            [ css
                [ width (px heightVal)
                , height (px heightVal)
                , backgroundColor (hex "#fff")
                , borderRadius (px 10)
                , position absolute
                , transforms [ translateXConditional, scale 1.1 ]
                , transition [ Transition.transform 300 ]
                ]
            ]
            []
        ]
