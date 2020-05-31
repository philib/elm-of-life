module Main exposing (..)

import Html exposing (Html, div, td, text, tr)
import List exposing (concatMap, filter, length, map, range)
type alias Point = { x: Int, y: Int}
type State = Alive | Dead
type alias Cell = { state: State, point: Point }
type alias Neighbours = List Cell
type alias Game = List (List Cell)

main = renderGame createGame

renderGame: Game -> Html msg
renderGame game = div [] (map renderRow game)

renderRow: List Cell -> Html msg
renderRow cells = tr [] (map renderCell cells)

renderCell: Cell -> Html msg
renderCell cell =
    case cell.state of
        Alive -> td [] [text "X"]
        Dead -> td [] [text "O"]

createGame: Game
createGame = map (\x -> map (\y -> createCell x y) (range 0 10) ) (range 0 10)

createCell: Int -> Int -> Cell
createCell x y = {state = Alive, point= { x=x, y=y}}

getNewGeneration: Game -> Game
getNewGeneration game = map (\row -> getNewGenerationList game row) game

getNewGenerationList: Game -> List Cell -> List Cell
getNewGenerationList game cells = map (\x -> getNewCellState x (getNeighbours x game)) cells

getNewCellState: Cell -> Neighbours -> Cell
getNewCellState cell neighbours =
    case cell.state of
        Dead ->
            if(List.length neighbours == 3) then
                cell
            else
                { state= Alive, point= cell.point}
        Alive ->
            if(List.length neighbours == 1 || length neighbours > 3) then
                { state = Dead, point = cell.point}
            else
                cell

getNeighbours: Cell -> Game -> Neighbours
getNeighbours cell allCells = filter (\c -> isNeighbour cell.point c.point) (concatMap identity allCells)

isNeighbour: Point -> Point -> Bool
isNeighbour a b = not (isSame a b) && ( (a.y == b.y + 1 || a.y == b.y - 1 || a.y == b.y) &&(a.x == b.x + 1 || a.x == b.x - 1 || a.x == b.x) )

isSame: Point -> Point -> Bool
isSame a b = (a.x == b.x) && (a.y == b.y)
