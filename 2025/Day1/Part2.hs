main = interact $ show . countZeroes . (map rotationToInt) . lines

-- Change to count amount of times zero is crossed
countZeroes :: [Integer] -> Integer
countZeroes xs = countZeroCrosses 50 xs

countZeroCrosses :: Integer -> [Integer] -> Integer
countZeroCrosses _ [] = 0
countZeroCrosses prev (x:xs) = let (next, crosses) = countCrosses prev x
                               in crosses + countZeroCrosses next xs

countCrosses :: Integer -> Integer -> (Integer, Integer)
countCrosses prev x = let
                          total = prev + x
                          crosses = (abs total) `div` 100
                          all_crosses = if total > 0 || prev == 0 then crosses else crosses + 1
                          next = total `mod` 100
                      in (next, all_crosses)

rotationToInt :: String -> Integer
rotationToInt ('L':num) = -1 * (read num)
rotationToInt ('R':num) = read num
rotationToInt s = error $ "bad line: " ++ s ++ "\n"

