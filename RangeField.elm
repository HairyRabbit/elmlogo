module RangeField exposing (..)

import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import Debug
import Json.Decode as Json exposing ((:=))

import Html.App as Html


-- Model

type Move
  = Step
  | Continuum


type alias Model =
  { value  : Float
  , position : Position
  , drag   : Maybe Drag
  }


type alias Drag =
  { start : Position
  , curr  : Position
  }


init : (Model, Cmd Msg)
init =
  ( { value  = 0
    , position = Position 0 0
    , drag   = Nothing
    }
  , Cmd.none
  )


-- Update

type Msg
  = NoOp
  | DragStart Position
  | DragAt    Position
  | DragEnd   Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DragStart xy ->
      ( { model | drag = Just <| Drag xy xy
        }
      , Cmd.none
      )

    DragAt xy ->
      let
        rangeX pos =
          if pos.x <= 0 then
            { pos | x = 0 }
          else if pos.x >= 200 then
            { pos | x = 200 }
          else
            pos
      in
        ( { model | drag = Maybe.map (\{ start } -> Drag start <| rangeX xy) model.drag }
        , Cmd.none
        )

    DragEnd _ ->
      let
        position =
          model.position
        pos =
          case model.drag of
            Nothing ->
              position
              
            Just { start, curr } ->
              Position
                (position.x + curr.x - start.x)
                0
      in
        ( { model | position = pos, drag = Nothing }
        , Cmd.none
        )
    _ ->
      (model, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions { drag } =
  case drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


-- View

view : Model -> Html Msg
view model =
  div [ class "range"
      , rangeStyle
      ]
    [ div [ class "line"
          , lineStyle
          ]
        [ div [ class "cursor"
              , cursorStyle model
              , on "mousedown" (Json.map DragStart Mouse.position)
              ] []
        ]
    ]


(=>) = (,)

rangeStyle : Attribute msg
rangeStyle =
  style
    [ "padding" => "2rem"
    ]
       
lineStyle : Attribute msg
lineStyle =
  style
    [ "position" => "relative"
    , "width" => "200px"
    , "height" => "2px"
    , "backgroundColor" => "#ddd"
    , "cursor" => "pointer"
    ]

cursorStyle : Model -> Attribute msg
cursorStyle { position, drag } =
  let
    x =
      case drag of
        Nothing ->
          position.x
        Just {start, curr} ->
          position.x + curr.x - start.x
  in
    style
      [ "position" => "absolute"
      , "width" => "1rem"
      , "height" => "1rem"
      , "borderRadius" => "50%"
      , "backgroundColor" => "rgb(76, 103, 194)"
      , "left" => "0"
      , "top" => "-0.5rem"
      , "transform" => ("translate3d(" ++ (toString x) ++ "px, 0, 0)")
      , "cursor" => "pointer"
      ]
  

-- MAIN

main : Program Never
main =
  Html.program
    { init   = init
    , update = update
    , view   = view
    , subscriptions = subscriptions
    }
