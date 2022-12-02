# F# Solutions for 2022 Advent of Code Challenges

Running this project with `dotnet run` will test all of the solutions and run them against the corresponding data files in `./Data`. Solutions based on that input data will be printed to STDOUT.

Solutions are organized into separate modules for each part of each day. Each solution module should contain three items:
- A function `run : 'input -> 'output` that contains the actual logic of the solution.
- An [FsCheck](https://github.com/fscheck/FsCheck) test using the given test input and output. If this test fails, a TypeInitializationException will be raised prior to running against production data.
- A function `solution : string -> string` that wraps the `run` function with the required logic to read the input data from a file and convert types as necessary. The input parameter is the path to the file containing the input data.
