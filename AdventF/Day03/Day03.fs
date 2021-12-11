namespace AdventF.Day03
    module Parser =
        open AdventF.Shared

        let private inputLines = stringLines "Day03"

        let rec private asInts charList = 
            match charList with
            | '0' :: tail -> 0 :: asInts tail
            | '1' :: tail -> 1 :: asInts tail
            | [] -> []
            | _ -> failwith "wrong number in input"

        let private parseLine (line : string) = Seq.toList (line.ToCharArray()) |> asInts;

        let rec private parseLines lines =
            match lines with
            | head :: tail -> parseLine head :: parseLines tail
            | [] -> []

        let parsedLines = inputLines |> parseLines

    module Solution =
        open Parser

        let rec calculateFirstColumnSum (l : int list list) =
            match l with
            | [] -> 0
            | head :: tail -> head.Head + (calculateFirstColumnSum tail)

        let stripFirst (l : int list) = l.Tail

        let rec stripFromAll l =
            match l with
            | [] -> []
            | head :: tail -> stripFirst head :: stripFromAll tail

        let rec columnSums l num = 
            match num with
            | 0 -> []
            | _ -> calculateFirstColumnSum l :: columnSums (stripFromAll l) (num - 1)

        let rec compareToBit comparison sums =
            match sums with
            | [] -> []
            | head :: tail ->
                if comparison head then 1 :: compareToBit comparison tail
                else 0 :: compareToBit comparison tail

        let rec intsToChars i =
            match i with
            | 0 :: tail -> '0' :: intsToChars tail
            | 1 :: tail -> '1' :: intsToChars tail
            | _ -> []

        let rec charsToString c =
            match c with
            | [] -> ""
            | x :: tail -> x.ToString() + charsToString tail

        let toInt64 (input : string) = System.Convert.ToInt64(input, 2)

        let intsToLong = intsToChars >> charsToString >> toInt64

        let inputSums = columnSums parsedLines parsedLines.Head.Length

        let gammaList = compareToBit ((<=) (parsedLines.Length / 2))
        let epsilonList = compareToBit ((>=) (parsedLines.Length / 2))

        let gamma = gammaList inputSums |> intsToLong
        let epsilon = epsilonList inputSums |> intsToLong

        let part1 = gamma * epsilon
        let part2 = ""