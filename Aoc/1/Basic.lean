def rawFile := IO.FS.readFile "Aoc/1/input.txt"

def parseRaw (rawFile: IO String): IO (List (Int × Int)) := do
  let raw ← rawFile
  let splitted := raw.splitOn "\n"
  let listOfTwoElementList := List.map (λ x => x.splitOn "   ") splitted
  let dropped := List.dropLast listOfTwoElementList
  let pairs := List.map (λ x => ((List.get! x 0).toInt!, (List.get! x 1).toInt!)) dropped
  pure pairs

set_option maxRecDepth 10000
#eval parseRaw rawFile

def input: List (Int × Int) := [(3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3)]

def transpose (input: List (Int × Int)) := input.foldl (fun (acc1, acc2) (x, y) => (acc1.append [x], acc2.append [y]) ) ([], [])

def solve (input) := transpose input
      |> (λ (a, b) => (List.mergeSort a, List.mergeSort b))
      |> (λ (a, b) => a.zip b)
      |> (λ l => l.map (λ (x, y) => (x - y).natAbs))
      |> List.foldl Nat.add 0

def solveIO (inputIO: IO (List (Int × Int))) : IO Nat := do
  let input ← inputIO
  pure $ solve input

#eval solveIO $ pure [(3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3)]

#eval solveIO $ parseRaw rawFile
