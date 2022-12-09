module Day9
open FParsec
open FsUnit

type Position = { X: int; Y: int}
type RopeSegment = { Head: Position; Tail: Position }
type Direction = Left | Right | Up | Down

let moveHead pos direction =
  match direction with
  | Left  -> { pos with X = pos.X - 1 }
  | Right -> { pos with X = pos.X + 1 }
  | Up    -> { pos with Y = pos.Y + 1 }
  | Down  -> { pos with Y = pos.Y - 1 }

let (/?) n d = if d = 0 then 0 else n / d

let moveTail newHead tail =
  match (newHead.X - tail.X), (newHead.Y - tail.Y) with
  | dx, dy when abs dx > 1 || abs dy > 1 ->
      { tail with
          X = tail.X + dx/?(abs dx)
          Y = tail.Y + dy/?(abs dy) }
  | _ -> tail

let move rope direction =
  let h = moveHead rope.Head direction
  { Head = h; Tail = moveTail h rope.Tail }

let testMovements =
  [|Right; Right; Right; Right
    Up; Up; Up; Up
    Left; Left; Left
    Down
    Right; Right; Right; Right
    Down
    Left; Left; Left; Left; Left
    Right; Right |]

let parseLine (input: string) : Direction * int =
  let directionParser =
    (pstring "R" >>% Right) <|> (pstring "L" >>% Left) <|> (pstring "U" >>% Up) <|> (pstring "D" >>% Down)
  let movementParser = directionParser .>> pstring " " .>>. pint32
  match run movementParser input with
  | Success(x,_,_) -> x
  | _ -> invalidOp $"Parser failed on line '{input}'"

let initialPosition = { Head = { X = 0; Y = 0 }; Tail = { X = 0; Y = 0 } }

module Part1 =

  let run =
    Array.scan move initialPosition
    >> Array.map (fun r -> r.Tail)
    >> Array.distinct
    >> Array.length

  testMovements |> run |> should equal 13

  let solution =
    System.IO.File.ReadAllLines
    >> Array.map parseLine
    >> Array.collect (fun (d,i) -> Array.init i (fun _ -> d))
    >> run
    >> string

module Part2 =

  let rec moveTails rope acc =
    match rope with
    | head::next::rest ->
        let newNext = moveTail head next
        moveTails (newNext::rest) (head::acc)
    | last::[] -> last::acc
    | [] -> invalidOp "Cannot move tails of an empty list."

  let moveLongRope (rope: list<_>) direction =
    let newHeadPos = moveHead rope.Head direction
    moveTails (newHeadPos::rope.Tail) []
    |> List.rev

  let testMovementStr =
    [|"R 5"
      "U 8"
      "L 8"
      "D 3"
      "R 17"
      "D 10"
      "L 25"
      "U 20"|]

  let initialRope = List.init 10 (fun _ -> { X = 0; Y = 0 })

  let run =
    Array.scan moveLongRope initialRope
    >> Array.map List.last
    >> Array.distinct
    >> Array.length

  let parse =
    Array.map parseLine
    >> Array.collect (fun (d,i) -> Array.init i (fun _ -> d))

  testMovements |> run |> should equal 1
  testMovementStr |> parse |> run |> should equal 36

  let solution =
    System.IO.File.ReadAllLines
    >> parse
    >> run
    >> string
