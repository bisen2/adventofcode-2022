module Program

// Solutions are represented as a function of type `string -> string`.
// The input string is a path to the data file. The output string is the answer.
// Test cases are defined below each `run` function. These test cases will throw an exception
//   during type initialization of the DayX.PartY module.

let solutions =
  [ ((1,1), Day1.Part1.solution); ((1,2), Day1.Part2.solution)
    ((2,1), Day2.Part1.solution); ((2,2), Day2.Part2.solution)
    ((3,1), Day3.Part1.solution); ((3,2), Day3.Part2.solution)
    ((4,1), Day4.Part1.solution); ((4,2), Day4.Part2.solution)
    ((5,1), Day5.Part1.solution); ((5,2), Day5.Part2.solution)
    ((6,1), Day6.Part1.solution); ((6,1), Day6.Part2.solution)
    ((8,1), Day8.Part1.solution); ((8,1), Day8.Part2.solution) ]

let runSolution ((day, part), solution) =
  $"{__SOURCE_DIRECTORY__}/Data/Day%d{day}.txt"
  |> solution
  |> printfn "Day %d Part %d Answer: %s" day part

[<EntryPoint>]
let main _ =
  solutions
  |> List.iter runSolution
  0
