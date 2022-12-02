module Day1
open FsUnit

let testInput =
  [| "1000"; "2000"; "3000"; ""; "4000"; ""; "5000";
    "6000"; ""; "7000"; "8000"; "9000"; ""; "10000" |]

let folder state input =
  match input, state with
  | "", _ -> 0 :: state
  | x, [] -> [ int x ]
  | x, y :: ys -> (int x + y) :: ys

module Part1 =

  let run =
    Array.fold folder []
    >> List.max

  testInput |> run |> should equal 24000

  let solution =
    System.IO.File.ReadAllLines
    >> run
    >> string

module Part2 =

  let run =
    Array.fold folder []
    >> List.sortDescending
    >> List.take 3
    >> List.sum

  testInput |> run |> should equal 45000

  let solution =
    System.IO.File.ReadAllLines
    >> run
    >> string
