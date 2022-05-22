module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix, every  )
import Update
import Json.Decode as Decode
import View exposing (view)
import Model exposing (..)
import Update exposing(update)

init: () -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { snake = []
            , currentDirection = Down
            , nextDirections = []
            , apples = []
            , gameStatus = Initializing
            , options = Options 40
            , score = 0
            }
    in
    ( model, Cmd.none )

decodeArrow : String -> Decode.Decoder String
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed "Up"
    "ArrowLeft" -> Decode.succeed "Left"
    "ArrowRight" -> Decode.succeed "Right"
    "ArrowDown" -> Decode.succeed "Down"
    " " -> Decode.succeed "Space"
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions model = 
  let
      delay = 200
  in
  case model.gameStatus of
    Playing -> 
      Sub.batch 
                [ every delay NextFrame 
                , Browser.Events.onKeyDown decodeKey
                ]

    _ ->
      Browser.Events.onKeyDown decodeKey


main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }