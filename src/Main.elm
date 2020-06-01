module Main exposing (..)

import Browser
import Delay exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (concatMap, filter, length, map, range)
import List.Extra
import Time
import Tuple
type alias Point = (Int, Int)
type State =  Alive | Dead
type Cell = Cell State Point
type alias AliveNeighbours = Int
type alias Game = Dict Point State
type alias Model = {paused: Bool, game:Game}
type Msg = ManualTick | Start | TimedTick Time.Posix | ToggleCellState Point

init : () -> (Model, Cmd Msg)
init _ =  ({paused = True, game = createGamePartial}, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg {paused, game} =
    case msg of
        ManualTick -> ({paused=True, game= getNewGeneration game}, Cmd.none)
        Start -> ({paused=if(paused) then False else True, game= game}, Cmd.none)
        TimedTick _ -> ({paused=paused, game= getNewGeneration game}, Cmd.none)
        ToggleCellState point -> ({paused=paused, game= toggleCellState point game}, Cmd.none)

view: Model -> Html Msg
view model = div [] [button [onClick ManualTick] [text "Step"],  button [onClick Start] [text "Start/Pause"], (renderGame model.game)]

subscriptions: Model -> Sub Msg
subscriptions {paused} = if(paused) then Sub.none else Time.every 400 TimedTick

main = Browser.element { init= init, view= view, update= update, subscriptions= subscriptions}

renderGame: Game -> Html Msg
renderGame game = statesToTable (gameToList game)

gameToList: Game -> List (List Cell)
gameToList game = map (\e->  map (\(_, cell)-> cell ) (Tuple.second e))(List.Extra.groupWhile (\(rowA, _) (rowB, _  ) -> rowA == rowB) (map (\(point, state)-> (getX point, Cell state point))(Dict.toList game)))

statesToTable: List (List Cell) -> Html Msg
statesToTable cells = table [style "border" "solid"] (map renderRow cells)

renderRow: List Cell -> Html Msg
renderRow cells = tr [style "border" "solid"] (map renderCell cells)

renderCell:  Cell -> Html Msg
renderCell (Cell state point) =
    case state of
        (Dead) -> td [onClick (ToggleCellState point)] [div [style "height" "10px", style "width" "10px", style "background-color" "grey"] []]
        (Alive)-> td [onClick (ToggleCellState point)] [div [style "height" "10px", style "width" "10px", style "background-color" "black"] []]

toggleCellState: Point -> Game -> Game
toggleCellState point game =
        Dict.map (\p s ->
            if (getX point == getX p && getY point == getY p) then
                toggle s
            else
                s
        ) game

getX: Point -> Int
getX p = Tuple.first p

getY: Point -> Int
getY p = Tuple.second p

toggle: State -> State
toggle state =
        case state of
            Alive -> Dead
            Dead -> Alive

createGamePartial: Game
createGamePartial = Dict.fromList (map (\x-> (x, Dead)) (createCoordinates 50))

createCoordinates: Int -> List Point
createCoordinates n = concatMap (\x -> map (\y -> (x,y)) (range 0 n)) (range 0 n)

getNewGeneration: Game -> Game
getNewGeneration game = Dict.map (\point state -> getNewCellState state (getNeighboursCount point game)) game

getNewCellState: State -> AliveNeighbours -> State
getNewCellState cell neighbours =
    case cell of
        Dead ->
            if(neighbours == 3) then
                Alive
            else
                cell
        Alive ->
            if(neighbours <= 1 || neighbours > 3) then
                Dead
            else
                cell

getNeighboursCount: Point -> Game -> AliveNeighbours
getNeighboursCount cell allCells =
    List.length (filter (\state-> state == Alive) (List.filterMap identity (map (\n -> Dict.get n allCells) (potentialNeighbours cell))))

potentialNeighbours: Point -> List Point
potentialNeighbours (x, y) = [(x - 1, y), (x + 1 , y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
