module Main

convertNumber : String -> Maybe Nat
convertNumber input =
  if all isDigit (unpack input)
  then Just (cast input)
  else Nothing

choose : (n : Nat) -> (k : Nat) -> Nat
choose Z Z = 1
choose _ Z = 1
choose (S n) (S k) =
  if n == k then
    1
  else
    choose n k + choose n (S k)

total choose_q : (n : Nat) -> (k : Nat) -> Double
choose_q Z Z = 1
choose_q _ Z = 1
choose_q Z _ = 1 -- for totality
choose_q (S n) (S k) = let recurse = (choose_q n k) in
                       ((cast (S n)) * recurse) / (cast (S k))

choose_maybe : (Maybe Nat) -> (Maybe Nat) -> Maybe Double
choose_maybe (Just n) (Just k) = Just (choose_q n k)
choose_maybe _ _ = Nothing

main : IO ()
main = do putStr "n = ?: "
          input1 <- getLine
          n <- pure (convertNumber(input1))
          putStr "k = ?: "
          input2 <- getLine
          k <- pure (convertNumber(input2))
          result <- pure (choose_maybe n k)
          putStrLn ("the result is: " ++ (show result))
