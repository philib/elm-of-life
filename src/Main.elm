module Main exposing (..)

import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (concatMap, filter, length, map, range)
import Random exposing (Generator)
type alias Point = { x: Int, y: Int}
type State = Alive | Dead
type alias Cell = { state: State, point: Point }
type alias Neighbours = List Cell
type alias Game = List (List Cell)
type Msg = Tick | ToggleCellState Cell

init : Game
init =  createGamePartial

update: Msg -> Game -> Game
update msg model =
    case msg of
        Tick -> getNewGeneration model
        ToggleCellState cell -> toggleCellState cell model


view: Game -> Html Msg
view model = div [] [button [onClick Tick] [text "Tick"], renderGame model]

main = Browser.sandbox { init= init, view= view, update= update }

renderGame: Game -> Html Msg
renderGame game = table [style "border" "solid"] (map renderRow game)

renderRow: List Cell -> Html Msg
renderRow cells = tr [style "border" "solid"] (map renderCell cells)

renderCell: Cell -> Html Msg
renderCell cell =
    case cell.state of
        Dead -> td [onClick (ToggleCellState cell)] [div [style "height" "10px", style "width" "10px", style "background-color" "grey"] []]
        Alive -> td [onClick (ToggleCellState cell)] [div [style "height" "10px", style "width" "10px", style "background-color" "black"] []]

toggleCellState: Cell -> Game -> Game
toggleCellState cell game =
    map (\rows ->
        map (\c ->
            if (cell.point.x == c.point.x && cell.point.y == c.point.y) then
                toggle cell
            else
                c
        ) rows
    ) game

toggle: Cell -> Cell
toggle {state, point} = {
    state= (
        case state of
            Alive -> Dead
            Dead -> Alive
           )
    ,point= point
    }


createGamePartial: Game
createGamePartial = map (\row -> map (\coords -> createCell coords Dead) row) (createCoordinates 50)

createCoordinates: Int -> List (List (Int, Int))
createCoordinates n = map (\x -> map (\y -> Tuple.pair x y)  (range 0 n)) (range 0 n)

createCell: (Int, Int) -> State -> Cell
createCell (x ,y) state = {state = state, point= { x=x, y=y}}

getNewGeneration: Game -> Game
getNewGeneration game = map (\row -> getNewGenerationList game row) game

getNewGenerationList: Game -> List Cell -> List Cell
getNewGenerationList game cells = map (\x -> getNewCellState x (getNeighbours x game)) cells

getNewCellState: Cell -> Neighbours -> Cell
getNewCellState cell neighbours =
    case cell.state of
        Dead ->
            if(aliveNeighbours neighbours == 3) then
                { state= Alive, point= cell.point}
            else
                cell
        Alive ->
            if(aliveNeighbours neighbours <= 1 || aliveNeighbours neighbours > 3) then
                { state = Dead, point = cell.point}
            else
                cell

aliveNeighbours: List Cell -> Int
aliveNeighbours cells = length (filter identity (map isAlive cells))

isAlive: Cell -> Bool
isAlive cell = cell.state == Alive

getNeighbours: Cell -> Game -> Neighbours
getNeighbours cell allCells = filter (\c -> isNeighbour cell.point c.point) (concatMap identity allCells)

isNeighbour: Point -> Point -> Bool
isNeighbour a b = not (isSame a b) && ( (a.y == b.y + 1 || a.y == b.y - 1 || a.y == b.y) &&(a.x == b.x + 1 || a.x == b.x - 1 || a.x == b.x) )

isSame: Point -> Point -> Bool
isSame a b = (a.x == b.x) && (a.y == b.y)
