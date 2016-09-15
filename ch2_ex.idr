palindrome : String -> Bool
palindrome s = s == reverse s

palindrome_no_case : String -> Bool
palindrome_no_case s =
  let lowercase = toLower s in
      length lowercase > 10 && lowercase == reverse lowercase

top_ten : Ord a => List a -> List a
top_ten ls =
  let sorted = reverse(sort ls) in
      take 10 sorted

over_length : Nat -> List String -> Nat
over_length over strs =
  let lengths = map length strs
      too_long = filter (> over) lengths in
      length too_long
