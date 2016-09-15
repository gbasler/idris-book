longer : String -> String -> String
longer w1 w2
  = let l1 = length w1
        l2 = length w2 in
        if l1 > l2 then w1 else w2
