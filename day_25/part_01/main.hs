
data ParseResult = Lock Thing | Key Thing
                   deriving Show

type Thing = (Integer, Integer, Integer, Integer, Integer)

process ls = processLines ls [] [] []

processLines [] locks keys _ = (locks, keys)

processLines ((line:ls) :: [String]) (locks :: [Thing]) (keys :: [Thing]) (temp :: [String]) =
    case line of
        "" ->
            case parseTemp temp of
                Lock lock -> processLines ls (lock : locks) keys []
                Key key -> processLines ls locks (key : keys) []

        _ -> processLines ls locks keys (line : temp)

parseTemp :: [String] -> ParseResult
parseTemp (temp :: [String]) =
    let middle = drop 1 (take 5 temp) in
        let middleSlots = arrayToFivetuple (countMiddle middle []) in
            if head temp == "#####" then 
                Lock middleSlots
            else 
                Key middleSlots

countMiddle [] acc = acc

countMiddle ((t:ts) :: [String]) (acc :: [Integer]) =
    let additions = map (\c -> if c == '#' then 1 else 0) t in
        countMiddle ts (zipWith (+) additions acc)

arrayToFivetuple :: [e] -> (e, e, e, e, e)
arrayToFivetuple [s1, s2, s3, s4, s5] = (s1, s2, s3, s4, s5)

main :: IO ()
main = do
    contents <- readFile "input_test.txt"

    let ls = lines contents

    let (slots, keys) = process ls

    print slots
    print keys
