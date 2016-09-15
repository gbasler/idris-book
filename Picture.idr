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

data Biggest = NoTriangle | Size Double

%name Biggest size, size1, size2

bigger : (size : Biggest) -> (size1 : Biggest) -> Biggest
bigger NoTriangle NoTriangle = NoTriangle
bigger NoTriangle (Size x) = Size x
bigger (Size x) NoTriangle = Size x
bigger (Size x) (Size y) = Size (max x y)

total biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive (Triangle x y)) = Size (area (Triangle x y))
biggestTriangle (Primitive shape) = NoTriangle
biggestTriangle (Combine pic pic1) = let size = biggestTriangle pic
                                         size1 = biggestTriangle pic1 in
                                         bigger size size1
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
