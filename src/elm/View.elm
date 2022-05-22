module View exposing (view, gameSize)

import Canvas exposing (Renderable, Shape, rect, shapes)
import Canvas.Settings exposing (..)
import Color
import Html exposing (..)
import Html.Attributes exposing (class, href, selected, style)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Model exposing (..)


view : Model -> Html Msg
view model =
    let
        wrapper =
            \body -> div [] [ headerView, body ]
    in
    case model.gameStatus of
        Initializing ->
            wrapper (initializingView model)

        Playing ->
            wrapper (gameView model)

        Paused ->
            wrapper (gameView model)

        GameOver ->
            wrapper (gameOverView model)


headerView : Html Msg
headerView =
    header []
        [ h1 [] [ text "Snake" ]
        ]
-- INITIALIZING VIEW


initializingView : Model -> Html Msg
initializingView model =
    div [ class "container" ]
        [ newGameForm model ]


newGameForm : Model -> Html Msg
newGameForm model =
    div [ class "game-form" ]
        [ div []
            [ span [] [ text "Game Size" ]
            , select [ onGameBoardSizeChanged ]
                [ option
                    [ selected (model.options.gameBoardSize == 40) ]
                    [ text "40" ]
                , option
                    [ selected (model.options.gameBoardSize == 50) ]
                    [ text "50" ]
                , option
                    [ selected (model.options.gameBoardSize == 60) ]
                    [ text "60" ]
                ]
            ]
        , button [ onClick StartGame ] [ text "Start Game" ]
        ]



onGameBoardSizeChanged : Html.Attribute Msg
onGameBoardSizeChanged =
    let
        targetValueDecoder =
            Decode.at [ "target", "value" ] Decode.string

        stringToMsg =
            \s -> GameBoardSizeChanged (Maybe.withDefault 0 (String.toInt s))

        decoder =
            Decode.map stringToMsg targetValueDecoder
    in
    on "change" decoder



-- GAME OVER VIEW


gameOverView : Model -> Html Msg
gameOverView model =
    div [ class "container" ]
        [ div []
            [ h2 []
                [ span [] [ text "Game Over | Score : " ]
                , span [] [ model.score |> String.fromInt |> text ]
                ]
            ]
        , gameBoardView model
        , h3 [] [ text "Play again? " ]
        , newGameForm model
        ]



-- GAME VIEW


gameView : Model -> Html Msg
gameView model =
    div [ class "container" ]
        [ h2 []
            [ span [] [ text "Score : " ]
            , span [] [ model.score |> String.fromInt |> text ]
            ]
        , gameBoardView model
        ]

 
gameBoardView : Model -> Html Msg
gameBoardView model =
    Canvas.toHtml ( (gameSize model).x, (gameSize model).y )
        []
        [ shapes
            [ fill (Color.rgb255 0x44 0x44 0x44) ]
            [ rect ( 0, 0 ) (toFloat (gameSize model).x) (toFloat (gameSize model).y) ]
        , snakeView model
        , foodView model
        ]


foodView : Model -> Renderable
foodView model =
    shapes [ fill (Color.rgb255 95 158 160) ]
        (List.map (\seg -> pointToRect model seg) model.apples)


snakeView : Model -> Renderable
snakeView model =
    shapes [ fill (Color.rgb255 159 96 157) ]
        (List.map (\seg -> pointToRect model seg) model.snake)


pointToRect : Model -> Point -> Shape
pointToRect model point =
    let
        cellWidth =
            toFloat gridSize

        cellHeight =
            toFloat gridSize

        pixelsX =
            toFloat point.x * cellWidth

        pixelsY =
            toFloat point.y * cellHeight
    in
    rect ( pixelsX, pixelsY ) cellWidth cellHeight


gameSize : Model -> Point
gameSize model =
    Point (model.options.gameBoardSize*gridSize)  (model.options.gameBoardSize*gridSize)

gridSize : Int 
gridSize = 20 
