module Utilities

#nowarn "40"
open System

type AlgorithmOption =
    | Lagrange
    | Linear
    | Both

let functionsToExecute (algorithm: AlgorithmOption) =
    match algorithm with
    | Lagrange -> [ InterpolationUtils.lagrangeRun, algorithm ]
    | Linear -> [ InterpolationUtils.linearRun, algorithm ]
    | Both ->
        [ InterpolationUtils.linearRun, Linear
          InterpolationUtils.lagrangeRun, Lagrange ]

let generator (start: double) (step: double) =
    let getPoint (num: int) = start + (double num) * step
    getPoint

let getPointGenerators (points: list<double * double>) step =
    [ (generator (fst points[1]) step, int ((fst points.Head - fst points[1]) / step))
      (generator (fst points[points.Length - 1]) step, int ((fst points.Head - fst points[points.Length - 1]) / step)) ]

let printValues (func: double -> double) algorithm (pointGeneratorN: (int -> double) * int) =
    async {
        let pointGenerator, n = pointGeneratorN

        { 0..n }
        |> Seq.map (fun i -> $"(%f{pointGenerator i};\t%f{func (pointGenerator i)})\n")
        |> Seq.fold (+) "x\t\ty\n"
        |> fun s -> printfn $"{algorithm} result: \n{s}"
    }

let rec inputValuesSeq =
    seq {
        let line = Console.ReadLine()

        match not (String.IsNullOrEmpty line) with
        | true ->
            let data = line.TrimEnd(';').Split(';')

            match data.Length >= 2 with
            | true ->
                let x, y =
                    match (data[0], data[1]) with
                    | xStr, yStr -> double xStr, double yStr

                yield (x, y)
            | _ -> yield! inputValuesSeq
        | false -> exit 0
    }
