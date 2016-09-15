import Data.Vect
my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = (my_reverse xs) ++ [x]

total my_reverse_tailrec : List a -> List a
my_reverse_tailrec xs = reverse_acc [] xs
  where
    reverse_acc : List a -> List a -> List a
    reverse_acc acc [] = acc
    reverse_acc acc (x :: xs) = reverse_acc (x :: acc) xs

total my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = (f x) :: (my_map f xs)

my_mapv : (a -> b) -> Vect n a -> Vect n b
my_mapv f [] = []
my_mapv f (x :: xs) = f x :: my_mapv f xs
