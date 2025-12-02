main = interact $ show . countZeroes . foldRotations . lines

foldRotations :: [String] -> [Integer]
foldRotations xs = scanl plusModRotation 50 xs

countZeroes :: [Integer] -> Integer
countZeroes (0:xs) = 1 + countZeroes xs
countZeroes (_:xs) = countZeroes xs
countZeroes _ = 0

plusModRotation :: Integer -> String -> Integer
plusModRotation num s = (num + (rotationToInt s)) `mod` 100

rotationToInt :: String -> Integer
rotationToInt ('L':num) = -1 * (read num)
rotationToInt ('R':num) = read num
rotationToInt s = error $ "bad line: " ++ s ++ "\n"

