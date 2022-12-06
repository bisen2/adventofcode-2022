module Day6
open FsUnit

let testInputs =
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    "bvwbjplbgvbhsrlpgdmjqwftvncz"
    "nppdvjthqldpwncqszvftbrmjlhg"
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" ]

let findUniqueOfLength length input =
  input
  |> Seq.windowed length
  |> Seq.findIndex (fun x -> x |> Seq.distinct |> Seq.length |> (=) (Seq.length x))
  |> (+) length

module Part1 =

  let run = findUniqueOfLength 4

  testInputs |> List.map run |> should equal [ 7; 5; 6; 10; 11 ]

  let solution =
    System.IO.File.ReadAllText
    >> run
    >> string

module Part2 =

  let run = findUniqueOfLength 14

  testInputs |> List.map run |> should equal [ 19; 23; 23; 29; 26 ]

  let solution =
    System.IO.File.ReadAllText
    >> run
    >> string
