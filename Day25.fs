module Day25

let data = (8458505L, 16050997L)

let solve1 data =

    let findLoopSize subject target =
        let rec findLoopSize' value n =
            if value = target then n
            else findLoopSize' (value * subject % 20201227L) (n + 1)
        findLoopSize' 1L 0

    let transformTimes subject times =
        let rec transformTimes' value n =
            if n = times then value
            else transformTimes' (value * subject % 20201227L) (n + 1)
        transformTimes' 1L 0

    transformTimes (snd data) <| findLoopSize 7L (fst data)

solve1 data
