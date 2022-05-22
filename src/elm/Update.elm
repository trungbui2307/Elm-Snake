module Update exposing (update)

import Array
import Browser.Events
import Char
import Model exposing (..)
import Random


{-| Just do nothing -}
none : model -> ( model, Cmd msg )
none model = ( model, Cmd.none )

{-| Bundles one command with the Model -}
withCmd : Cmd msg -> model -> ( model, Cmd msg )
withCmd cmd model = ( model, cmd )

{-| Bundles some commands with the Model -}
withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model = ( model, Cmd.batch cmds )

keyDown : String -> Model -> Maybe Msg
keyDown key model =
  case Debug.log "key" key of
        "ArrowLeft" ->
            Just (DirectionChange Left)

        "ArrowUp" ->
            Just (DirectionChange Up)

        "ArrowRight" ->
            Just (DirectionChange Right)

        "ArrowDown" ->
            Just (DirectionChange Down)
        " " ->
            case model.gameStatus of
                Model.GameOver ->
                    Just StartGame

                _ ->
                    Just TogglePause
        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
        NextFrame _ -> 
            nextFrame model

        KeyDown keyCode ->
            let
                maybeMessage =
                    keyDown keyCode model
            in
            case maybeMessage of
                Just nextMsg ->
                    update nextMsg model

                Nothing ->
                    ( model, Cmd.none )


        NewApple 1 index ->
            let
                emptyCells =
                    findEmptyCells model

                newFood =
                    Maybe.withDefault
                        (Point 1 1)
                        (Array.get index (Array.fromList emptyCells))
            in
            ( { model | apples = newFood :: model.apples }, Cmd.none )

        NewApple n index ->
            let
                emptyCells =
                    findEmptyCells model

                newFood =
                    Maybe.withDefault
                        (Point 1 1)
                        (Array.get index (Array.fromList emptyCells))

                lengthEmptyCellsLeft =
                    List.length emptyCells - 1
            in
            ( { model | apples = newFood :: model.apples }
            , Random.generate (NewApple (n - 1)) (Random.int -1 lengthEmptyCellsLeft)
            )

        StartGame ->
            let
                gameBoardSize =
                    model.options.gameBoardSize

                emptyCellsCount =
                    (gameBoardSize * gameBoardSize) - 3

                foodToGenerate = 1
            in
            ( { model
                | gameStatus = Playing
                , snake = [ Point 0 0, Point 1 0, Point 2 0 ]
                , currentDirection = Right
                , apples = []
                , nextDirections = []
                , score = 0
              }
            , Random.generate (NewApple foodToGenerate) (Random.int -1 emptyCellsCount)
            )

        GameBoardSizeChanged gameBoardSize ->
            let
                oldOptions =
                    model.options

                newOptions =
                    { oldOptions | gameBoardSize = gameBoardSize }
            in
            ( { model | options = newOptions }, Cmd.none )

        DirectionChange direction ->
            ( { model
                | nextDirections = updateNextDirections model direction
              }
            , Cmd.none
            )

        TogglePause ->
            case model.gameStatus of
                Playing ->
                    ( { model | gameStatus = Paused }, Cmd.none )

                Paused ->
                    ( { model | gameStatus = Playing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



updateNextDirections : Model -> Direction -> List Direction
updateNextDirections model direction =
    if List.any (\d -> d == direction) model.nextDirections then
        model.nextDirections

    else
        direction :: model.nextDirections

oppositeDirections : Direction -> Direction -> Bool
oppositeDirections d1 d2 =
    if (d1 == Left || d2 == Left) && (d1 == Right || d2 == Right) then
        True

    else if (d1 == Up || d2 == Up) && (d1 == Down || d2 == Down) then
        True

    else
        False


nextFrame : Model -> ( Model, Cmd Msg )
nextFrame model = 
    let
        nextDirection = 
            case List.head (List.reverse model.nextDirections) of 
                Just d -> 
                    if oppositeDirections d model.currentDirection then 
                        model.currentDirection
                    else 
                        d 
                Nothing -> 
                    model.currentDirection
        advancedSnake = 
            List.foldl
                (\queue newSnake ->  advanceSnakeSegment nextDirection model.snake newSnake queue)
                []
                (List.reverse model.snake)

        snakeHead = 
            List.head(List.reverse advancedSnake)

        nbrEmptyCell =
            (model.options.gameBoardSize * model.options.gameBoardSize) - List.length advancedSnake - List.length model.apples

        nextDirections =
            List.take
                (List.length model.nextDirections - 1)
                model.nextDirections

        hasDied =
            detectDeath snakeHead advancedSnake model.options.gameBoardSize

        ateApple =
            List.head
                (List.filter
                    (\apple -> Just apple == snakeHead)
                    model.apples
                )


    in
    if hasDied then
        ( { model | gameStatus = GameOver }, Cmd.none )

    else
        case ateApple of
            Just f ->
                ( { model
                    | snake = updateSnake advancedSnake 
                    , currentDirection = nextDirection
                    , nextDirections = nextDirections
                    , apples = List.filter (\f2 -> f /= f2) model.apples
                    , score = model.score + 100
                  }
                , Random.generate (NewApple 1) (Random.int -1 nbrEmptyCell)
                )

            Nothing ->
                ( { model
                    | snake = advancedSnake
                    , currentDirection = nextDirection
                    , nextDirections = nextDirections
                  }
                , Cmd.none
                )

updateSnake : List Point -> List Point
updateSnake mySnake  =
    let
        lastSegment =
            List.head mySnake

        growSpeed = 1
    in
    case lastSegment of
        Just s ->
            List.append (List.repeat growSpeed s) mySnake

        Nothing ->
            mySnake


detectDeath : Maybe Point -> List Point -> Int -> Bool
detectDeath snakeHead snake gameBoardSize =
    case snakeHead of
        Just sh ->
            let
                collisionWithBorder =
                    sh.x < 0 || sh.y < 0 || sh.x >= gameBoardSize || sh.y >= gameBoardSize

                snakeTail =
                    List.take (List.length snake - 1) snake

                eatSelf =
                    List.any (\seg -> seg == sh) snakeTail
            in
            collisionWithBorder || eatSelf

        Nothing -> False

findEmptyCells : Model -> List Point
findEmptyCells model =
    let
        gameBoardSize =
            model.options.gameBoardSize

        totalCells =
            gameBoardSize * gameBoardSize

        nonEmptyCells =
            List.append model.snake model.apples

        range =
            List.range 0 (totalCells - 1)

        allCells =
            List.map (\c -> Point (c // gameBoardSize) (modBy gameBoardSize c)) range
    in
    List.filter (\c -> not (List.any (\c2 -> c == c2) nonEmptyCells)) allCells


advanceSnakeSegment : Direction -> List Point -> List Point -> Point -> List Point
advanceSnakeSegment direction oldSnake newSnake segment =
    let
        aheadIndex = (List.length newSnake) + 1

        aheadSegment =
            Array.get aheadIndex <| (Array.fromList oldSnake)
    in
    if List.length newSnake == 0 then
        case direction of
            Up -> [ segment, Point segment.x (segment.y - 1) ]
            Down -> [ segment, Point segment.x (segment.y + 1) ]
            Left -> [ segment, Point (segment.x - 1) segment.y ]
            Right -> [ segment, Point (segment.x + 1) segment.y ]

    else if List.length newSnake == List.length oldSnake then
        newSnake

    else
        segment :: newSnake

