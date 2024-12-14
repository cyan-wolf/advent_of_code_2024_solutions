
// Parse the first and second parts of the input.
(Dictionary<int, HashSet<int>>, List<int[]>) ReadInput() 
{
    bool readingFirstPart = true;
    // First part of the input.
    Dictionary<int, HashSet<int>> mustHaveBefore = []; 
    // Second part of the input.
    List<int[]> updateLines = [];

    foreach (var line in File.ReadLines("input.txt"))
    {
        if (line.Length == 0) 
        {
            readingFirstPart = false;
            continue;
        }

        if (readingFirstPart) 
        {
            var nums = line.Split('|')
                .Select(int.Parse)
                .ToArray();

            if (!mustHaveBefore.ContainsKey(nums[1]))
            {
                mustHaveBefore.Add(nums[1], [nums[0]]);
                continue;
            }
            mustHaveBefore[nums[1]].Add(nums[0]);
        }
        else
        {
            var nums = line.Split(',')
                .Select(int.Parse)
                .ToArray();

            updateLines.Add(nums);
        }
    }

    return (mustHaveBefore, updateLines);
}

int GetSumOfMidpointsOfValids(Dictionary<int, HashSet<int>> mustHaveSeenBefore, List<int[]> updateLines)
{
    int sum = 0;

    foreach (var uptLine in updateLines)
    {
        HashSet<int> seen = [];
        HashSet<int> presentInLine = new(uptLine);
        bool lineIsValid = true;

        foreach (var num in uptLine)
        {
            if (!lineIsValid)
            {
                break;
            }

            seen.Add(num);

            HashSet<int> mustHaveSeen = [];
            if (mustHaveSeenBefore.ContainsKey(num))
            {
                mustHaveSeen = mustHaveSeenBefore[num];
            }
            
            foreach (var reqSeen in mustHaveSeen)
            {
                // If a number is in the 'update line', but is out of order,
                // that means the line is not valid.
                if (presentInLine.Contains(reqSeen) && !seen.Contains(reqSeen))
                {
                    lineIsValid = false;
                    break;
                }
            }
        }
        
        // If a line was valid, then its midpoint should be added to the sum.
        if (lineIsValid)
        {
            int midpoint = uptLine[uptLine.Length / 2];
            sum += midpoint;
        }
    }

    return sum;
}

var (mustHaveBefore, updateLines) = ReadInput();
int sum = GetSumOfMidpointsOfValids(mustHaveBefore, updateLines);

Console.WriteLine(sum);