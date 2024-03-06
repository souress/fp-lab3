module Program

[<EntryPoint>]
let main args =
    try
        let commandLineOptions = Parser.parseCommandLine args
        Executor.execute Utilities.inputValuesSeq commandLineOptions
        0
    with ex ->
        printfn $"{ex.Message}"
        -1
