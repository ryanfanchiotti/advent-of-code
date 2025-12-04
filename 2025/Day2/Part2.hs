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
                (first, second) = splitAt half_i str
            in first == second
