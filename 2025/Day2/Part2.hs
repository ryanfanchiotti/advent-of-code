main = interact $ show . sum . (map sumSymRange) . mapToIntegers . splitCommas

-- copied from GHC.Utils.Misc impl
split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

splitCommas :: String -> [String]
splitCommas = split ',' 

mapToIntegers :: [String] -> [(Integer, Integer)]
mapToIntegers = map $ intsListToTuple . (map read) . (split '-') 

intsListToTuple :: [Integer] -> (Integer, Integer)
intsListToTuple (x:y:[]) = (x, y)
intsListToTuple xs = error $ "invalid parse: " ++ (show xs)

sumSymRange :: (Integer, Integer) -> Integer
sumSymRange (start, end) = sum [x | x <- [start..end], isValid x]

isValid :: Integer -> Bool
isValid x = let
                str = show x
                len = length str
                half_i = len `div` 2
            in or $ map (allChunksEq str) [1..half_i]
    where
        allChunksEq s i = allEq $ chunksOf i s

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

allEq :: Eq a => [a] -> Bool
allEq (x:xs) = all (==x) xs
allEq _ = error "empty list"
