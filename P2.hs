--haskell project

getWidth :: (Num b) => [a] -> b
getWidth [] = 0
getWidth (x:xs) = 1 + getWidth xs 

first' :: String -> String
first' "" = "Empty string dummy"
first' mouse@(x:xs) = "First letter of " ++ mouse ++ "  is " ++ xs

addTwo = 5 + 4

getRed :: (Num a) => (a,b,c) -> a
getRed (a,b,c) = a^2

getGreen :: (Num b) => (a,b,c) -> b
getGreen (a,b,c) = b^2

getBlue :: (Num c) => (a,b,c) -> c
getBlue (a,b,c) = c^2

--readTrip :: (Num a,b,c) => (a,b,c)

