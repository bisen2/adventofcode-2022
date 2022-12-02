module Day2
open FsUnit

let testInput =
  [|'A', 'Y';
    'B', 'X';
    'C', 'Z'; |]

type Shape =
  | Rock
  | Paper
  | Scissors
  member this.Score =
    match this with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  static member ofOpponentCode = function
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | x -> invalidOp $"Character '{x}' is not a valid opponent shape."

type Round =
  { OpponentShape : Shape
    PlayerShape : Shape }
  member this.Outcome =
    match this.PlayerShape, this.OpponentShape with
    | x, y when x = y -> 3 // draw
    | Rock, Scissors
    | Scissors, Paper
    | Paper, Rock -> 6 // win
    | _ -> 0 // loss
  member this.Score = this.PlayerShape.Score + this.Outcome

module Part1 =

  type Shape with
    static member ofPlayerCode = function
      | 'X' -> Rock
      | 'Y' -> Paper
      | 'Z' -> Scissors
      | x -> invalidOp $"Character '{x}' is not a valid player shape."

  type Round with
      static member Parse(opponent, player) =
        { OpponentShape = Shape.ofOpponentCode opponent
          PlayerShape = Shape.ofPlayerCode player }

  let run =
    Array.map (Round.Parse >> fun x -> x.Score)
    >> Array.sum

  testInput |> run |> should equal 15

  let solution =
    System.IO.File.ReadAllLines
    >> Array.map (fun s -> s.[0], s.[2])
    >> run
    >> string

module Part2 =

  type Outcome =
    | Win
    | Lose
    | Draw
    static member ofCode = function
      | 'X' -> Lose
      | 'Y' -> Draw
      | 'Z' -> Win
      | x -> invalidOp $"Character '{x}' is not a known outcome character."

  type Shape with
    member this.forOutcome outcome =
      match this, outcome with
      | x, Draw -> x
      | Rock, Win -> Paper
      | Rock, Lose -> Scissors
      | Paper, Win -> Scissors
      | Paper, Lose -> Rock
      | Scissors, Win -> Rock
      | Scissors, Lose -> Paper

  type Round with
    static member Parse(opponent, outcome) =
      let opShape = Shape.ofOpponentCode opponent
      { OpponentShape = opShape
        PlayerShape = opShape.forOutcome (Outcome.ofCode outcome) }

  let run =
    Array.map (Round.Parse >> fun x -> x.Score)
    >> Array.sum

  testInput |> run |> should equal 12

  let solution =
    System.IO.File.ReadAllLines
    >> Array.map (fun s -> s.[0], s.[2])
    >> run
    >> string
