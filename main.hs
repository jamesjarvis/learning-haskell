showResult :: Int -> String

showResult n =
  "The result is " ++ show n

showAreaOfCircle :: Float -> String

showAreaOfCircle n =
  "The area of a circle with radius " ++ show n ++ "cm is about " ++ show (pi * n^2) ++ "cm^2"
