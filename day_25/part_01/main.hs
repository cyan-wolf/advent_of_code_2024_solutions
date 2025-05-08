import GHC.Num (integerFromInt)
import System.Environment (getArgs)

-- Represents a fixed sized list of slot heights.
type SlotHeights = [Integer]

-- Represents whether a list of slot heights belongs to a lock or a key.
data ParseResult = Lock SlotHeights | Key SlotHeights
                   deriving Show

-- Processes a list of lines to parse a list of locks and a list of keys.
process :: [String] -> ([SlotHeights], [SlotHeights])
process ls = processLines ls [] []
    where
        processLines [] locks keys = (locks, keys)

        processLines (ls :: [String]) (locks :: [SlotHeights]) (keys :: [SlotHeights]) =
            let remainingInput = drop 8 ls in 
                case parseSlotHeights (take 7 ls) of
                    Lock lock -> processLines remainingInput (lock : locks) keys
                    Key key -> processLines remainingInput locks (key : keys)

-- Checks several lines to parse a lock or a key.
parseSlotHeights :: [String] -> ParseResult
parseSlotHeights (temp :: [String]) =
    let middle = take 5 (drop 1 temp) in
        let middleSlots = countColumns middle in
            if head temp == "#####" then 
                Lock middleSlots
            else 
                Key middleSlots

-- Counts how many '#' symbols appear in each position (column) 
-- of the given rows.
countColumns :: [String] -> SlotHeights
countColumns rows =
    let rowLen = length (head rows) in
        countMiddleAux rows (replicate rowLen 0)

    where 
        countMiddleAux [] acc = acc

        countMiddleAux ((r:rest) :: [String]) (acc :: [Integer]) =
            let additions = map (\c -> if c == '#' then 1 else 0) r in
                countMiddleAux rest (zipWith (+) additions acc)

-- Returns whether the given slots fit in each other.
slotsFit :: SlotHeights -> SlotHeights -> Bool
slotsFit lock key = 
    let size = integerFromInt $ length lock in 
        all (\(lockSlot, keySlot) -> (lockSlot + keySlot) <= size) (zip lock key)

-- Checks every combination of lock and key from the given lists 
-- and returns how many combinations are non-overlapping.
findNonOverlappingCombinations locks keys = 
    length [(lock, key) | lock <- locks, 
                   key <- keys, 
                   slotsFit lock key]

main :: IO ()
main = do 
    args <- getArgs
    case args of 
        [filepath] -> do
            contents <- readFile filepath
            print (uncurry findNonOverlappingCombinations $ process (lines contents))

        _ -> putStrLn "error: a file path must be provided"
