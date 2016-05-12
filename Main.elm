module Main           exposing (..)

import Html           exposing (..)
import Html.Events    exposing (..)
import Html.Attributes
import Html.App       as Html
import Svg            exposing (..)
import Svg.Attributes exposing (..)
import String
import Debug


-- MODEL

type alias Model =
  { colors : LogoColor
  }

type alias Color =
  { h : Float
  , s : Float
  , l : Float
  }

type alias LogoColor =
  { color1 : Color
  , color2 : Color
  , color3 : Color
  , color4 : Color
  }

color1 : Color
color1 =
  { h = 222
  , s = 14
  , l = 41
  }

color2 : Color
color2 =
  { h = 193
  , s = 51
  , l = 59
  }

color3 : Color
color3 =
  { h = 93
  , s = 62
  , l = 52
  }

color4 : Color
color4 =
  { h = 43
  , s = 100
  , l = 47
  }

initLogoColor : LogoColor
initLogoColor =
  { color1 = color1
  , color2 = color2
  , color3 = color3
  , color4 = color4
  }

init : (Model, Cmd Msg)
init =
  ( { colors = initLogoColor }
  , Cmd.none
  )

-- UPDATE

type Msg
  = NoOp
  | ChangeColor Int String

colorToString : Color -> String
colorToString c =
  "hsl(" ++ (toString c.h) ++ "," ++ (toString c.s) ++ "%," ++ (toString c.l) ++ "%)"


updateColor : Color -> Float -> Color
updateColor c h =
  { c | h = h }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeColor idx val ->
      case idx of
        1 ->
          let
            colors =
              model.colors
            c =
              updateColor model.colors.color1 <| Result.withDefault 0 (String.toFloat val)

            lc =
              { colors | color1 = c }
          in
            ( { model | colors = lc }
            , Cmd.none
            )
        2 ->
          let
            colors =
              model.colors
            c =
              updateColor model.colors.color2 <| Result.withDefault 0 (String.toFloat val)

            lc =
              { colors | color2 = c }
          in
            ( { model | colors = lc }
            , Cmd.none
            )
        3 ->
          let
            colors =
              model.colors
            c =
              updateColor model.colors.color3 <| Result.withDefault 0 (String.toFloat val)
            lc =
              { colors | color3 = c }
          in
            ( { model | colors = lc }
            , Cmd.none
            )
        4 ->
          let
            colors =
              model.colors
            c =
              updateColor model.colors.color4 <| Result.withDefault 0 (String.toFloat val)
            lc =
              { colors | color4 = c }
          in
            ( { model | colors = lc }
            , Cmd.none
            )
        _ ->
          (model, Cmd.none)
    _ ->
      (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ logoView model
    , input
        [ Html.Attributes.type' "range"
        , Html.Attributes.min "1"
        , Html.Attributes.max "360"
        , onInput <| ChangeColor 1
        ] []
    , input
        [ Html.Attributes.type' "range"
        , Html.Attributes.min "1"
        , Html.Attributes.max "360"
        , onInput <| ChangeColor 2
        ] []
    , input
        [ Html.Attributes.type' "range"
        , Html.Attributes.min "1"
        , Html.Attributes.max "360"
        , onInput <| ChangeColor 3
        ] []
    , input
        [ Html.Attributes.type' "range"
        , Html.Attributes.min "1"
        , Html.Attributes.max "360"
        , onInput <| ChangeColor 4
        ] []
    ]

logoView : Model -> Html msg
logoView model =
  Svg.svg
    [ width "200"
    , height "200"
    , viewBox "0 0 323.141 322.95"
    ]
    [ polygon
        [ fill <| colorToString model.colors.color4 --"#34495E"
        , points "161.649,152.782 231.514,82.916 91.783,82.916"
        ] []
    , polygon
        [ fill <| colorToString model.colors.color3
        , points "8.867,0 79.241,70.375 232.213,70.375 161.838,0"
        ] []
    , rect
        [ fill <| colorToString model.colors.color3
        , x "192.99"
        , y "107.392"
        , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
        , width "107.676"
        , height "108.167"
        ] []
    , polygon
        [ fill <| colorToString model.colors.color2
        , points "323.298,143.724 323.298,0 179.573,0"
        ] []
    , polygon
        [ fill <| colorToString model.colors.color1
        , points "152.781,161.649 0,8.868 0,314.432"
        ] []
    , polygon
        [ fill <| colorToString model.colors.color4
        , points "255.522,246.655 323.298,314.432 323.298,178.879"
        ] []
    , polygon
        [ fill <| colorToString model.colors.color2
        , points "161.649,170.517 8.869,323.298 314.43,323.298"
        ] []
    ]


-- MAIN

main : Program Never
main =
  Html.program
    { init   = init
    , update = update
    , view   = view
    , subscriptions = \_ -> Sub.none
    }
