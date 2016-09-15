avg : String -> Double
avg str
  = let ws = words str
        tot = map length ws in
        cast (sum (tot)) / cast (length ws)
