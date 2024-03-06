module Program

open System

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

[<EntryPoint>]
let main args =
    try
        let commandLineOptions = Parser.parseCommandLine args
        Executor.execute inputValuesSeq commandLineOptions
        0
    with ex ->
        printfn $"{ex.Message}"
        -1
