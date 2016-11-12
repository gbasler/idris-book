module Main

convertNumber : String -> Maybe Nat
convertNumber input =
  if all isDigit (unpack input)
  then Just (cast input)
  else Nothing

choose : (n : Nat) -> (k : Nat) -> Nat
choose Z Z = 1
choose n Z = 1
choose (S n) (S k) =
  if n == k then
    1
  else
    choose n k + choose n (S k)

payoff_c : Double -> Double
payoff_c s = let k = 100 in
             if(s > k) then s - k else 0.0

--price_terminal : (n : Nat) -> (i : Nat)

--price_path :

price : (s0 : Double) -> (r : Double) -> (n : Nat) -> Double
price s0 r n
  = let u = 1.0 + 1.0 / (cast n)
        d = 1.0 / u
        qu = ((1.0 + r) - d) / (u - d)
        qd = (u - (1.0 + r)) / (u - d) in
        d

main : IO ()
main = let result = choose 10 9 in
       putStrLn ("the price is: " ++ (show result))


--main : IO ()
--main = do putStr "Number of steps:"
--          input1 <- getLine
--          n <- pure (convertNumber(input1))
--          n0 <- n
--          let s = 100.0
--          let k = 100.0
--          let r = 0.0
          --let u = 1.0 + 1.0 / (cast n)
          --let d = 1.0 / u
          --let p = price? S K r u d
          --pure (price (cast n))
--          putStrLn ("longer word is: ")
