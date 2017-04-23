module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
  then pure (Just (cast input))
  else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStr(show(guesses) ++ ": ")
  Just guessed <- readNumber
  | Nothing => do putStrLn "Type a number to guess!"
                  guess target guesses
  if guessed > target
  then do putStrLn "Guess is too high!"
          guess target (succ guesses)
  else if guessed < target then do putStrLn "Guess is too low!"
                                   guess target (succ guesses)
  else putStrLn "You nailed it!"


main : IO ()
main = do
  putStrLn "Guess a number!"
  t <- time
  let r = (cast (t `mod` 100))
  guess r Z
