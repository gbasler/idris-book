--word_length Nil = []
--word_length (hd :: tl) = length hd :: word_length tl
word_length : List String -> List Nat
word_length [] = []
word_length (word :: words) = length word :: word_length words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)
