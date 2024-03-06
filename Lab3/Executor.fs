module Executor

open Parser
open Utilities

let rec recursivePoints (input: 'T seq) windowSize (list: 'T list) =
    seq {
        let pair = Seq.head input

        let newList =
            match list.Length with
            | 0 -> [ pair ]
            | _ -> pair :: List.truncate (windowSize - 1) list

        match newList.Length < 2 with
        | true -> yield! recursivePoints input windowSize newList
        | _ ->
            yield newList
            yield! recursivePoints input windowSize newList
    }



let execute (inputSeq: (double * double) seq) (options: CommandLineOptions) =
    recursivePoints inputSeq options.Window []
    |> Seq.iter (fun pairs ->
        let functionsWithInputs =
            options.Algorithm
            |> functionsToExecute
            |> List.map (fun (func, algorithm) -> (func pairs, algorithm))

        functionsWithInputs
        |> Seq.iter (fun (act, algorithm) ->
            let pointGenerators = getPointGenerators pairs options.Step

            match algorithm, pairs.Length with
            | Linear, _ -> printValues act algorithm pointGenerators[0] |> Async.RunSynchronously
            | Lagrange, l when l = options.Window ->
                printValues act algorithm pointGenerators[1] |> Async.RunSynchronously
            | _ -> ()))
