import Data.Vect

word_lengths : Vect len String -> Vect len Nat
word_lengths [] = ?word_lengths_rhs_1
word_lengths (x :: xs) = ?ww :: word_lengths xs

--word_lengths [] = Nil
--word_lengths (word :: words) = length word :: word_lengths words
