import Lean.Data.HashMap

-- Convenience function for turning a 2-element list into a 2-tuple.
def listToTuple [Inhabited α] (list : List α) : α × α :=
  match list with
  | [a1, a2] => (a1, a2)
  | _ => unreachable!

-- Prepare the array of string lines into an array of integer tuples.
def prepareLines (lines : Array String) : Array (Int × Int) :=
  lines.map (fun line => listToTuple $ (line.splitOn "   ").map (String.toInt!))

-- Generate a hash map that counts the frequency of each
-- integer in the given array.
def toFreqMap (nums : Array Int) : Lean.HashMap Int Int :=
  let empty : Lean.HashMap Int Int := Lean.HashMap.empty

  nums.foldl (fun acc num =>
    let count := acc.findD num 0
    acc.insert num (count + 1)
  ) (empty)

def solution (lines : Array String) : Int :=
  let lines' := prepareLines lines
  let (first, second) := Array.unzip lines'

  let freqMap := toFreqMap second

  first.foldl (fun acc num =>
    let freq := freqMap.findD num 0
    acc + num * freq
  ) 0

def main : IO Unit := do
  IO.FS.lines "input.txt" >>= (IO.println ∘ solution)
