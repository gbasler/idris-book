import Data.Vect
create_empties : Vect n (Vect 0 elem)
create_empties = Vect.replicate _ []

transpose_helper : (x : Vect n elem) -> (xs_trans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transpose_helper [] [] = []
transpose_helper (x :: xs) (y :: ys) = (x :: y) :: transpose_helper xs ys

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
              zipWith (::) x xs_trans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = zipWith (zipWith (+)) xs ys

mult_row : Num a => (x : Vect m a) -> (ys : Vect p (Vect m a)) -> Vect p a
mult_row x [] = []
mult_row x (y :: ys) = sum (zipWith (*) x y) :: mult_row x ys

multMatrix_rhs : Num a => (xs : Vect n (Vect m a)) -> (ys_transposed : Vect p (Vect m a)) -> Vect n (Vect p a)
multMatrix_rhs [] _ = []
multMatrix_rhs (x :: xs) ys = mult_row x ys :: multMatrix_rhs xs ys

total multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = let ys_transposed = transpose_mat ys in
           multMatrix_rhs xs ys_transposed

vec_length : Vect m elem -> Nat
vec_length xs = foo _ xs
  where
    foo : (n : Nat) -> Vect n elem -> Nat
    foo n xs = n
