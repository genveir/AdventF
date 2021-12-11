namespace AdventF
    module Main =

        [<EntryPoint>]
        let main args = 
            printfn "part 1: %s" (Day02.Solution.part1.ToString())
            printfn "part 2: %s" (Day02.Solution.part2.ToString())
            0