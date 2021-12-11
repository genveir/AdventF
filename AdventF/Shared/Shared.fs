namespace AdventF
    module Shared = 

        let private readLines path = System.IO.File.ReadLines(sprintf "../../../%s/%s.txt" path path)
        
        let rec private asInts input acc = 
            match input with
            | [] -> acc
            | head :: tail -> asInts tail ((head |> int) :: acc)
        
        let stringLines path : string list = List.ofSeq (readLines path)
        let numberLines path : int list = asInts (stringLines path) []