import Data.List (sort)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let procLines = map processLine (lines content)
    let (lst1, lst2) = unzip procLines
    let dist = sumDistPaired lst1 lst2

    print dist

processLine :: String -> (Integer, Integer)
processLine = listToTuple . lineToProcessedList
    where
        lineToProcessedList = map read . words
        listToTuple [a, b] = (a, b)

-- Sorts each list, pairs them, and finds the 
-- (absolute) difference between each pair. 
-- Returns the sum of the differences.
sumDistPaired :: [Integer] -> [Integer] -> Integer
sumDistPaired lst1 lst2 = sum pairSums
    where
        pairs = zip (sort lst1) (sort lst2)
        pairSums = map (\(a, b) -> abs (b - a)) pairs
