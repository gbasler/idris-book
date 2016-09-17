import Data.Vect

-- how do I add the show interface here?
data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1, tree2

total insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left y right) = case compare x y of
                                    LT => Node (insert x left) y right
                                    EQ => orig
                                    GT => Node left y (insert x right)

total listToTree : Ord a => List a -> Tree a
listToTree list = listToTree2 list Empty
  where
    listToTree2 : Ord a => List a -> Tree a -> Tree a
    listToTree2 [] tree = tree
    listToTree2 (x :: xs) tree = listToTree2 xs (insert x tree)

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = (treeToList left) ++ [x] ++ (treeToList right)

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

%name Expr e, e1, e2

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add e e1) = evaluate e + evaluate e1
evaluate (Sub e e1) = evaluate e - evaluate e1
evaluate (Mult e e1) = evaluate e * evaluate e1

total maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing j@(Just x) = j
maxMaybe j@(Just x) Nothing = j
maxMaybe (Just x) (Just y) = Just(max x y)

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle x y) = x * y / 2.0
area (Rectangle x y) = x * y
area (Circle x) = x * x * pi

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

total biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape@(Triangle x y)) = Just(area shape)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic pic1) =
  let a = biggestTriangle pic
      a1 = biggestTriangle pic1 in
      maxMaybe a a1
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
          (Primitive (Triangle 2 4))
testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
          (Primitive (Circle 4))
