def List.sequence {m α} [Monad m] (lst: List (m α)) : m (List α) := List.mapM id lst

def parse (raw : String) : Option (List (Nat × Nat)) :=
  raw
    |>.splitOn "\n"
    |>.map (String.splitOn · "   ")
    |>.map (List.map String.toNat?)
    |>.map List.sequence
    |>.filter Option.isSome
    |>.sequence
    |>.map (List.map (λ x => match x with | [a, b] => some (a, b) | _ => none))
    |>.map List.sequence
    |>.join

def rawInput := IO.FS.readFile "Aoc/Day1/input.txt"
-- #eval parse <$> rawInput

def fakeParsedInput: List (Nat × Nat) := [(3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3)]

def solve1 (input: List (Nat × Nat)) : Nat :=
  input
    |>.unzip
    |> λ (a, b) => List.map (λ (x, y) => Int.natAbs (x - y)) (a.mergeSort.zip b.mergeSort)
    |>.sum

-- #eval solve1 fakeParsedInput
-- #eval (Option.map solve1 ∘ parse) <$> rawInput


def solve2 (input : List (Nat × Nat)) : Nat :=
  input
    |>.unzip
    |> λ (a, b) => a.map (λ x => x * List.count x b)
    |>.sum


#eval solve2 fakeParsedInput
#eval (Option.map solve2 ∘ parse) <$> rawInput
