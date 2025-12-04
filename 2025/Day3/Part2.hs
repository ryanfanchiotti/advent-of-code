import Data.Char

main = interact $ show . sum . (map getMaxNum) . ((map . map) (toInteger . digitToInt)) . lines

getMaxNum :: [Integer] -> Integer
getMaxNum xs = getTwelveMax [0 | _ <- [0..11]] xs $ length xs

-- get twelve max numbers in order
-- where do you insert into prevs? max of amount left and where it fits
-- (the first number it's bigger than)
getTwelveMax :: [Integer] -> [Integer] -> Int -> Integer
getTwelveMax prevs (x:xs) left = let
                                    f_b = firstBiggerIndex x prevs
                                    pos = max f_b $ 12 - left
                                    news = insertDrop prevs pos x
                                 in getTwelveMax news xs (left - 1)
getTwelveMax prevs [] _ = foldl (\acc d -> acc * 10 + d) 0 prevs 

-- drop only if needed
insertDrop :: [a] -> Int -> a -> [a]
insertDrop xs i num = let 
                    (zs, ys) = splitAt i xs
                    full = zs ++ [num]
                  in take 12 full

firstBiggerIndex :: Integer -> [Integer] -> Int
firstBiggerIndex num xs = fIndex 0 num xs
    where
        fIndex i _ [] = i
        fIndex i num (x:xs) = if num > x then i else fIndex (i + 1) num xs
