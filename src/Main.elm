module Main exposing (..)

type alias Point = { x: Int, y: Int}
type State = Alive | Dead
type alias Cell = { state: State, point: Point }
type alias Neighbours = List Cell
type alias Game = List Cell

getNewGeneration: Game -> Game
getNewGeneration game = List.map (\x -> getNewCellState x (getNeighbours x game)) game

getNewCellState: Cell -> Neighbours -> Cell
getNewCellState cell neighbours =
    case cell.state of
        Dead ->
            if(List.length neighbours == 3) then
                cell
            else
                { state= Alive, point= cell.point}
        Alive ->
            if(List.length neighbours == 1 || List.length neighbours > 3) then
                { state = Dead, point = cell.point}
            else
                cell

getNeighbours: Cell -> Game -> Neighbours
getNeighbours cell allCells = List.filter (\c -> isNeighbour cell.point c.point) allCells

isNeighbour: Point -> Point -> Bool
isNeighbour a b = not (isSame a b) && ( (a.y == b.y + 1 || a.y == b.y - 1 || a.y == b.y) &&(a.x == b.x + 1 || a.x == b.x - 1 || a.x == b.x) )

isSame: Point -> Point -> Bool
isSame a b = (a.x == b.x) && (a.y == b.y)
