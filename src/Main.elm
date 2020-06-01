module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, p, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (concatMap, filter, length, map, range)
import List.Extra
import Time
import Tuple


type alias Point =
    ( Int, Int )


type State
    = Alive
    | Dead


type Cell
    = Cell State Point


type alias AliveNeighboursCount =
    Int


type alias Game =
    Dict Point State


type alias Model =
    { paused : Bool, game : Game }


type Msg
    = ManualTick
    | Start
    | TimedTick Time.Posix
    | ToggleCellState Point
    | Reset


type alias Color =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { paused = True, game = initialGame }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { paused, game } =
    case msg of
        ManualTick ->
            ( { paused = True, game = getNextGeneration game }, Cmd.none )

        Start ->
            ( { paused =
                    if paused then
                        False

                    else
                        True
              , game = game
              }
            , Cmd.none
            )

        TimedTick _ ->
            ( { paused = paused, game = getNextGeneration game }, Cmd.none )

        ToggleCellState point ->
            ( { paused = paused, game = toggleCellState point game }, Cmd.none )

        Reset ->
            ( { paused = True, game = initialGame }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Game of Life" ]
        , div []
            [ text "Play God by killing or bringing cells back to life. Just click in the grid. Grey cells are dead, black cells are alive."
            , p []
                [ text "Use the button to perform a single step or to start/stop the game of live." ]
            ]
        , div []
            [ p []
                [ button [ onClick Start ] [ text "Start/Pause" ]
                , button [ onClick ManualTick ] [ text "Step" ]
                , button [ onClick Reset ] [ text "Reset" ]
                ]
            ]
        , div []
            [ renderGame model.game ]
        ]


subscriptions : Model -> Sub Msg
subscriptions { paused } =
    if paused then
        Sub.none

    else
        Time.every 400 TimedTick


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


renderGame : Game -> Html Msg
renderGame game =
    renderTable (transformToRows game)


transformToRows : Game -> List (List Cell)
transformToRows game =
    map (\e -> map (\( _, cell ) -> cell) (Tuple.second e)) (List.Extra.groupWhile (\( rowA, _ ) ( rowB, _ ) -> rowA == rowB) (map (\( point, state ) -> ( Tuple.first point, Cell state point )) (Dict.toList game)))


renderTable : List (List Cell) -> Html Msg
renderTable cells =
    table
        [ style "border" "1px solid black"
        , style "border-collapse" "collapse"
        ]
        (map renderRow cells)


renderRow : List Cell -> Html Msg
renderRow cells =
    tr [ style "border" "1px solid black" ] (map renderCell cells)


renderCell : Cell -> Html Msg
renderCell (Cell state point) =
    renderRowCell point <|
        case state of
            Dead ->
                "grey"

            Alive ->
                "black"


renderRowCell : Point -> Color -> Html Msg
renderRowCell point color =
    td
        [ onClick (ToggleCellState point)
        , style "height" "10px"
        , style "width" "10px"
        , style "border" "1px solid black"
        , style "border-collapse" "collapse"
        , style "background-color" color
        ]
        []


toggleCellState : Point -> Game -> Game
toggleCellState point game =
    Dict.update point (\state -> Maybe.andThen toggle state) game


toggle : State -> Maybe State
toggle state =
    case state of
        Alive ->
            Just Dead

        Dead ->
            Just Alive


initialGame : Game
initialGame =
    Dict.fromList (map (\x -> ( x, Dead )) (createCoordinates 150 55))


createCoordinates : Int -> Int -> List Point
createCoordinates n m =
    concatMap (\x -> map (\y -> ( x, y )) (range 0 n)) (range 0 m)


getNextGeneration : Game -> Game
getNextGeneration game =
    Dict.map (\point state -> getNewCellState state (getNeighboursCount point game)) game


getNewCellState : State -> AliveNeighboursCount -> State
getNewCellState cell neighbours =
    case cell of
        Dead ->
            if neighbours == 3 then
                Alive

            else
                cell

        Alive ->
            if neighbours <= 1 || neighbours > 3 then
                Dead

            else
                cell


getNeighboursCount : Point -> Game -> AliveNeighboursCount
getNeighboursCount point game =
    List.length (filter (\state -> state == Alive) (List.filterMap identity (map (\p -> Dict.get p game) (potentialNeighbours point))))


potentialNeighbours : Point -> List Point
potentialNeighbours ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]
