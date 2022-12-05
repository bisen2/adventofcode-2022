module Day4
open FsUnit
open FParsec

let testInput =
  [|(2,4), (6,8)
    (2,3), (4,5)
    (5,7), (7,9)
    (2,8), (3,7)
    (6,6), (4,6)
    (2,6), (4,8) |]

let parseLine input =
  let rangeParser = pint32 .>> pstring "-" .>>. pint32
  let parser = rangeParser .>> pstring "," .>>. rangeParser
  match run parser input with
  | Success(x,_,_) -> x
  | _ -> invalidOp $"Line '{input}' does not fit the expected pattern."

module Part1 =

  let hasSubset ((s1, e1), (s2, e2)) =
    ([s1 .. e1], [s2 .. e2])
    |> fun (r1, r2) -> Set.ofList r1, Set.ofList r2
    |> fun (s1, s2) -> Set.isSubset s1 s2 || Set.isSubset s2 s1

  let run =
    Array.where hasSubset
    >> Array.length

  testInput |> run |> should equal 2

  let (|>.) = pipe2

  let solution =
    System.IO.File.ReadAllLines
    >> Array.map parseLine
    >> run
    >> string

module Part2 =

  let hasOverlap ((s1, e1), (s2, e2))=
    ([s1 .. e1], [s2 .. e2])
    |> fun (r1, r2) -> Set.ofList r1, Set.ofList r2
    |> fun (s1, s2) -> Set.intersect s1 s2
    |> Set.isEmpty
    |> not

  let run =
    Array.where hasOverlap
    >> Array.length

  testInput |> run |> should equal 4

  let solution =
    System.IO.File.ReadAllLines
    >> Array.map parseLine
    >> run
    >> string
