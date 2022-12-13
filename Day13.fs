module Day13
open FParsec
open FsUnit

type Payload =
  | Int of int
  | List of list<Payload>
type Packet = list<Payload>

type Order = Correct | Incorrect | Equal

let rec ordered (lhs, rhs) =
  match lhs, rhs with
  // Int on both sides:
  | Int l, Int r when l < r -> Correct
  | Int l, Int r when l > r -> Incorrect
  | Int l, Int r when l = r -> Equal
  // List on both sides
  | List [], List [] -> Equal
  | List [], List (_::_) -> Correct
  | List (_::_), List [] -> Incorrect
  | List (l::ls), List (r::rs) ->
      match ordered(l, r) with
      | Equal -> ordered(List ls, List rs)
      | order -> order
  // One Int, one List
  | Int l, List rs -> ordered (List [Int l], List rs)
  | List ls, Int r -> ordered (List ls, List [Int r])
  | _ -> invalidOp $"Ordering not defined for {lhs} vs {rhs}"

let packetParser =
  let listParser, listParserRef = createParserForwardedToRef<Payload, unit>()
  listParserRef.Value <-
    between (pchar '[') (pchar ']')
      (sepBy (pint32 |>> Int <|> listParser) (pchar ','))
    |>> List
  listParser |>> function List ps -> ps | p -> [p]

let testInput = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""

module Part1 =

  let parser =
    let pairParser =
      packetParser .>> restOfLine true .>>. packetParser
    sepEndBy pairParser (restOfLine true >>. restOfLine true)

  let run input =
    match run parser input with
    | Failure(err,_,_) -> invalidOp $"Parser failed with error '{err}'"
    | Success(pairs,_,_) ->
        pairs
        |> List.map (fun (l,r) -> List l, List r)
        |> List.map ordered
        |> List.indexed
        |> List.sumBy (fun (i,x) -> x |> function Correct -> i+1 | _ -> 0)

  testInput |> run |> should equal 13

  let solution =
    System.IO.File.ReadAllText
    >> run
    >> string

module Part2 =

  let parse (lines: string) =
    lines.Split System.Environment.NewLine
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.map (run packetParser)
    |> Seq.choose (function | Success(x,_,_) -> Some x | _ -> None)

  let run input =
    let sorter x y =
      match ordered(x,y) with
      | Correct -> -1
      | Equal -> 0
      | Incorrect -> 1
    let sortedPackets =
      parse input
      |> Seq.map List
      |> Seq.append [ List[List[Int 2]]; List[List[Int 6]]]
      |> Seq.sortWith sorter
    let div1 = Seq.findIndex ((=) List[List[Int 2]]) sortedPackets
    let div2 = Seq.findIndex ((=) List[List[Int 6]]) sortedPackets
    (div1 + 1) * (div2 + 1)

  testInput |> run |> should equal 140

  let solution =
    System.IO.File.ReadAllText
    >> run
    >> string
