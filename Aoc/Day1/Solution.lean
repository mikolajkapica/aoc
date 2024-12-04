instance : Monad List where
  pure := List.singleton
  bind := List.flatMap

def List.sequence {m α} [Monad m] (lst: List (m α)) : m (List α) := List.mapM id lst

def parse (raw : String) : Option (List (Int × Int)) :=
  raw.splitOn "\n"
    |> List.map (String.splitOn · "   ")
    |> List.map (List.map String.toInt?)
    |> List.map List.sequence
    |> List.filter Option.isSome
    |> List.sequence
    |> Option.map (List.map (λ x => match x with | [a, b] => some (a, b) | _ => none))
    |> Option.map List.sequence
    |> Option.join

def solve (input: List (Int × Int)) : Nat :=
  input.unzip
    |> λ (a, b) => List.map (λ (x, y) => (x - y).natAbs) (a.mergeSort.zip b.mergeSort)
    |> List.sum

def fakeRawInput: String := "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
#eval solve <$> parse fakeRawInput

def fakeParsedInput: List (Int × Int) := [(3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3)]
#eval solve fakeParsedInput

def parseAndSolve : String → Option Nat :=
   Option.map solve ∘ parse

def rawInput := IO.FS.readFile "Aoc/Day1/input.txt"
#eval parseAndSolve <$> rawInput
