import Data.Char

main = interact $ show . sum . (map getMaxNum) . ((map . map) digitToInt) . lines

getMaxNum :: [Int] -> Int
getMaxNum xs = getTwoMax 0 0 xs

-- get two highest, with first occurring before second
-- works greedily, returning first digit * 10 + second digit
getTwoMax :: Int -> Int -> [Int] -> Int
getTwoMax p1 p2 [] = p1 * 10 + p2
getTwoMax p1 p2 (x:xs)
    -- for the case where there is an extra element left, we can always use x
    | x > p1, (_:_) <- xs = getTwoMax x 0 xs
    | x > p2 = getTwoMax p1 x xs
    | otherwise = getTwoMax p1 p2 xs
