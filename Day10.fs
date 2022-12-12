module Day10
open FsUnit

type Operation =
  | NoOp
  | AddX of int

let runOp state op =
  match op with
  | AddX x -> state + x
  | NoOp -> state

let parseLine i (line: string) =
  match line.Split " " with
  | [| "addx"; v |] -> [ i, NoOp; i, AddX (System.Int32.Parse v) ]
  | _ -> [ i, NoOp ]

let testInput =
  [|"addx 15"
    "addx -11"
    "addx 6"
    "addx -3"
    "addx 5"
    "addx -1"
    "addx -8"
    "addx 13"
    "addx 4"
    "noop"
    "addx -1"
    "addx 5"
    "addx -1"
    "addx 5"
    "addx -1"
    "addx 5"
    "addx -1"
    "addx 5"
    "addx -1"
    "addx -35"
    "addx 1"
    "addx 24"
    "addx -19"
    "addx 1"
    "addx 16"
    "addx -11"
    "noop"
    "noop"
    "addx 21"
    "addx -15"
    "noop"
    "noop"
    "addx -3"
    "addx 9"
    "addx 1"
    "addx -3"
    "addx 8"
    "addx 1"
    "addx 5"
    "noop"
    "noop"
    "noop"
    "noop"
    "noop"
    "addx -36"
    "noop"
    "addx 1"
    "addx 7"
    "noop"
    "noop"
    "noop"
    "addx 2"
    "addx 6"
    "noop"
    "noop"
    "noop"
    "noop"
    "noop"
    "addx 1"
    "noop"
    "noop"
    "addx 7"
    "addx 1"
    "noop"
    "addx -13"
    "addx 13"
    "addx 7"
    "noop"
    "addx 1"
    "addx -33"
    "noop"
    "noop"
    "noop"
    "addx 2"
    "noop"
    "noop"
    "noop"
    "addx 8"
    "noop"
    "addx -1"
    "addx 2"
    "addx 1"
    "noop"
    "addx 17"
    "addx -9"
    "addx 1"
    "addx 1"
    "addx -3"
    "addx 11"
    "noop"
    "noop"
    "addx 1"
    "noop"
    "addx 1"
    "noop"
    "noop"
    "addx -13"
    "addx -19"
    "addx 1"
    "addx 3"
    "addx 26"
    "addx -30"
    "addx 12"
    "addx -1"
    "addx 3"
    "addx 1"
    "noop"
    "noop"
    "noop"
    "addx -9"
    "addx 18"
    "addx 1"
    "addx 2"
    "noop"
    "noop"
    "addx 9"
    "noop"
    "noop"
    "noop"
    "addx -1"
    "addx 2"
    "addx -37"
    "addx 1"
    "addx 3"
    "noop"
    "addx 15"
    "addx -21"
    "addx 22"
    "addx -6"
    "addx 1"
    "noop"
    "addx 2"
    "addx 1"
    "noop"
    "addx -10"
    "noop"
    "noop"
    "addx 20"
    "addx 1"
    "addx 2"
    "addx 2"
    "addx -6"
    "addx -11"
    "noop"
    "noop"
    "noop" |]

module Part1 =

  let run =
    Array.mapi parseLine
    >> List.concat
    >> List.unzip
    >> fun (ids,ops) -> ids, List.scan runOp 1 ops
    >> fun (ids,states) -> List.zip ids states
    // >> List.scan runOp 1
    // >> List.indexed
    // >> List.sumBy (fun (i,s) -> if i % 40 - 20 = 0 then i * s else 0)

  testInput |> run |> should equal 13140

  let solution =
    System.IO.File.ReadAllLines
    >> run
    >> string
