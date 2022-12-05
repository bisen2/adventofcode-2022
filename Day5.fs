module Day5
open FParsec
open FsUnit

let testInput =
  [ ['N'; 'Z']
    ['D'; 'C'; 'M']
    ['P'] ],
  [ 1, 2, 1
    3, 1, 3
    2, 2, 1
    1, 1, 2 ]

type StackContent =
  | Crate of char
  | Space

let firstCrate : Parser<_,unit> = pstring "[" >>. letter .>> pstring "]" |>> Crate
let crate : Parser<_,unit> = pstring " [" >>. letter .>> pstring "]" |>> Crate
let space : Parser<_,unit> = pstring "    " >>% Space

let stackParser = sepEndBy (many (crate <|> firstCrate <|> space)) newline

let parseStacks (lines: array<string>) =
  lines
  |> String.concat System.Environment.NewLine
  |> run stackParser
  |> function
    | Failure(err,_,_) -> invalidOp $"Parser failed with error {err}"
    | Success(x,_,_) ->
        let len = x |> List.map List.length |> List.max
        let pad l = l @ (List.init (len - List.length l) (fun _ -> Space))
        x
        |> List.map pad
        |> List.transpose
        |> List.map (List.choose (function | Crate c -> Some c | _ -> None))

let moveParser =
  tuple3
    (pstring "move " >>. pint32)
    (pstring " from " >>. pint32)
    (pstring " to " >>. pint32)

let parseMoves (lines: array<string>) =
  lines
  |> Array.map (run moveParser)
  |> Array.choose (function | Success(x,_,_) -> Some x | _ -> None)
  |> Array.toList

let parse (lines: array<string>) =
  lines |> Array.where (fun s -> s.Contains "[") |> parseStacks,
  lines |> Array.where (fun s -> s.Contains "move") |> parseMoves

module Part1 =

  let rec performMove (stacks: list<list<char>>) (qty: int, from: int, to': int) =
    if qty = 0 then stacks
    else
      match stacks[from - 1] with
      | [] -> invalidOp $"Cannot remove from stack {from} because it is empty."
      | x::xs ->
          stacks
          |> List.updateAt (from - 1) xs
          |> List.updateAt (to' - 1) (x :: stacks[to' - 1])
          |> fun s -> performMove s (qty - 1, from, to')

  let run (state, moves) =
    moves
    |> List.fold performMove state
    |> List.map List.head
    |> List.toArray
    |> fun cs -> System.String(cs)

  testInput |> run |> should equal "CMZ"

  let solution =
    System.IO.File.ReadAllLines
    >> parse
    >> run

module Part2 =

  let performMove (stacks: list<list<char>>) (qty: int, from: int, to': int) =
    stacks
    |> List.updateAt (from - 1) (stacks[from-1][qty..])
    |> List.updateAt (to' - 1) (stacks[from-1][0..qty-1] @ stacks[to'-1])

  let run (state, moves) =
    moves
    |> List.fold performMove state
    |> List.map List.head
    |> List.toArray
    |> fun cs -> System.String(cs)

  testInput |> run |> should equal "MCD"

  let solution =
    System.IO.File.ReadAllLines
    >> parse
    >> run
