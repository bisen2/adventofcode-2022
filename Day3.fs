module Day3
open FsUnit

let testInput =
  [|"vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    "ttgJtRGJQctTZtZT"
    "CrZsJsPPZsGzwwsLwLmpwMDw"|]

let priority (item: char) =
  if System.Char.IsUpper item then (int item) - 38
  elif System.Char.IsLower item then int item - 96
  else invalidOp $"Character '{item}' is not a valid item code."

module Part1 =

  let bagPriority (bag: string) =
    (bag.Substring(0, bag.Length/2), bag.Substring(bag.Length/2))
    |> fun (c1, c2) -> Seq.where (fun i -> Seq.contains i c2) c1
    |> Seq.map priority
    |> Seq.head

  let run =
    Array.map bagPriority
    >> Array.sum

  testInput |> run |> should equal 157

  let solution =
    System.IO.File.ReadAllLines
    >> run
    >> string

module Part2 =

  let folder state (elfId, elfBag) =
    if elfId % 3 = 0 then [elfBag] :: state
    else (elfBag :: List.head state) :: List.tail state

  let createGroups : list<string> -> list<string * string * string> =
    List.indexed
    >> List.fold folder []
    >> List.map (fun es -> es.[0], es.[1], es.[2])

  let findBadges (elf1, elf2, elf3) =
    Seq.where (fun i -> Seq.contains i elf1 && Seq.contains i elf2) elf3
    |> Seq.head

  let run =
    Array.toList
    >> createGroups
    >> List.map (findBadges >> priority)
    >> List.sum

  testInput |> run |> should equal 70

  let solution =
    System.IO.File.ReadAllLines
    >> run
    >> string
