namespace AdventF.Day02
    module Parser =
        open AdventF.Shared
    
        type direction = 
        | forward = 1
        | up = 2
        | down = 3

        type SubMarineMove = { Direction : direction; Amount : int }
    
        let private parseDirection input =
            match input with
            | "forward" -> direction.forward
            | "up" -> direction.up
            | "down" -> direction.down
            | _ -> failwith "can't parse"
    
        let private splitLine (line : string) = Seq.toList (line.Split(' '))
    
        let private assignLine line =
            match line with
            | direction :: amount :: [] -> { Direction = (parseDirection direction); Amount = int amount }
            | _ -> failwith "can't parse submarine"
    
        let private parseLine line = assignLine (splitLine line)
    
        let rec private parseLines lines =
            match lines with
            | head :: tail -> parseLine head :: parseLines tail
            | [] -> []

        let parsedLines = stringLines "Day02" |> parseLines

    module Solution =
        open Parser

        type Position = { X : int ; Y : int ; Aim : int }
        
        let ZeroPos : Position = { X = 0; Y = 0; Aim = 0 }

        let DoMoveP1 input initial = 
            match input.Direction with
            | direction.forward -> { X = initial.X + input.Amount; Y = initial.Y ; Aim = 0 }
            | direction.up -> { X = initial.X; Y = initial.Y - input.Amount ; Aim = 0}
            | direction.down -> { X = initial.X; Y = initial.Y + input.Amount ; Aim = 0}
            | _ -> failwith "Unknown direction"

        let DoMoveP2 input initial = 
            match input.Direction with
            | direction.forward -> { X = initial.X + input.Amount ; Y = initial.Y + initial.Aim * input.Amount; Aim = initial.Aim }
            | direction.up -> { X = initial.X; Y = initial.Y; Aim = initial.Aim - input.Amount }
            | direction.down -> {X = initial.X; Y = initial.Y; Aim = initial.Aim + input.Amount }
            | _ -> failwith "Unknown direction"


        let rec DoMoves input acc moveFunc =
            match input with
            | head :: tail -> DoMoves tail (moveFunc head acc) moveFunc
            | _ -> acc

        let SolveWith = DoMoves parsedLines ZeroPos

        let GetScore pos = pos.X * pos.Y

        let part1 = SolveWith DoMoveP1 |> GetScore
        let part2 = SolveWith DoMoveP2 |> GetScore