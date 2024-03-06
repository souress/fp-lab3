module Parser

open Utilities

type CommandLineOptions =
    { Step: double
      Window: int
      Algorithm: AlgorithmOption }

let parseAlgorithm (algorithmStringValue: string) : AlgorithmOption =
    match algorithmStringValue with
    | "Lagrange" -> Lagrange
    | "Linear" -> Linear
    | "Both" -> Both
    | _ -> invalidArg "algorithm" "Unexpected algorithm! Possible values: [Lagrange; Linear; Both]"

let parseCommandLine (args: string array) : CommandLineOptions =
    match args with
    | [| step; window; algorithm |] ->
        assert (double step > 0)
        assert (int window >= 2)

        { Step = double step
          Window = int window
          Algorithm = parseAlgorithm algorithm }
    | _ -> failwith $"Expected 3 commandline arguments. Got {args.Length} arguments"
