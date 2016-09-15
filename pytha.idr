pytha : Double -> Double -> Double
pytha x y = sqrt (square x + square y)
  where
  square : Double -> Double
  square x = x * x
