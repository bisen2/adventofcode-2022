module Day8
open FsUnit

// This could probably be cleaner by actually using an Array2D...

let testInput =
  [|[| 3; 0; 3; 7; 3 |]
    [| 2; 5; 5; 1; 2 |]
    [| 6; 5; 3; 3; 2 |]
    [| 3; 3; 5; 4; 9 |]
    [| 3; 5; 3; 9; 0 |]|]

module Part1 =

  let isUnobstructed (map: array<array<int>>) (x,y) =
    let maxX = (map |> Array.map Array.length |> Array.max) - 1
    let maxY = Array.length map - 1
    if x=0 || y=0 || x = maxX || y = maxY then
      true
    else
      let west = (map[y][..x-1] |> Array.max) < map[y][x]
      let east = (map[y][x+1..] |> Array.max) < map[y][x]
      let north = (map[..y-1] |> Array.map (Array.item x) |> Array.max) < map[y][x]
      let south = (map[y+1..] |> Array.map (Array.item x) |> Array.max) < map[y][x]
      north || east || south || west

  let run (input: array<array<int>>) =
    let xDim = input |> Array.map Array.length |> Array.max
    let yDim = input |> Array.length

    Array.allPairs [|0 .. xDim - 1|] [|0 .. yDim - 1|]
    |> Array.map (isUnobstructed input)
    |> Array.where ((=) true)
    |> Array.length

  testInput |> run |> should equal 21

  let solution =
    System.IO.File.ReadAllLines
    >> Array.map Seq.toArray
    >> Array.map (Array.map (string >> System.Int32.Parse))
    >> run
    >> string

module Part2 =

  let viewScore (elev: int) (view: array<int>) =
    let rec viewScoreImpl soFar elev = function
    | x::xs when x < elev -> viewScoreImpl (soFar + 1) elev xs
    | [] -> soFar
    | _ -> soFar + 1
    view
    |> Array.toList
    |> viewScoreImpl 0 elev

  let viewDistance (map: array<array<int>>) (x,y) =
    let westView = map[y][..x-1] |> Array.rev
    let eastView = map[y][x+1..]
    let northView = (map[..y-1] |> Array.map (Array.item x)) |> Array.rev
    let southView = (map[y+1..] |> Array.map (Array.item x))
    let elev = map[y][x]

    let westViewScore = viewScore elev westView
    let eastViewScore = viewScore elev eastView
    let northViewScore = viewScore elev northView
    let southViewScore = viewScore elev southView

    northViewScore * eastViewScore * southViewScore * westViewScore

  let run (input: array<array<int>>) =
    let xDim = input |> Array.map Array.length |> Array.max
    let yDim = input |> Array.length

    Array.allPairs [|0 .. xDim - 1|] [|0 .. yDim - 1|]
    |> Array.map (viewDistance input)
    |> Array.max

  testInput |> run |> should equal 8

  let solution =
    System.IO.File.ReadAllLines
    >> Array.map Seq.toArray
    >> Array.map (Array.map (string >> System.Int32.Parse))
    >> run
    >> string
