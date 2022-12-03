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
  if System.Char.IsUpper item then int item - 38
  elif System.Char.IsLower item then int item - 96
  else invalidOp $"Character '{item}' is not a valid item code."

module Part1 =

  let bagPriority =
    Seq.splitInto 2
    >> Seq.map Set.ofSeq
    >> Set.intersectMany
    >> Set.map priority
    >> Seq.sum

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

  let createGroups elves =
    elves
    |> List.indexed
    |> List.fold folder []

  let findBadges : list<string> -> char =
    List.map Set.ofSeq
    >> Set.intersectMany
    >> Seq.head

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
