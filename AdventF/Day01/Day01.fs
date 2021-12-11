namespace AdventF.Day01
    module Solution =

        open AdventF.Shared

        let rec findHigher (input : int list) : int =
            match input with
            | head :: second :: tail -> 
                if head > second then 1 + findHigher (second :: tail)
                else findHigher (second :: tail)
            | _ -> 0

        let rec findSliding (input : int list) : int = 
            match input with
            | head :: second :: third :: fourth :: tail ->
                if head > fourth then 1 + findSliding (second :: third :: fourth :: tail)
                else findSliding (second :: third :: fourth :: tail)
            | _ -> 0

        let part1 = findHigher (numberLines "Day01")
        let part2 = findSliding (numberLines "Day01")