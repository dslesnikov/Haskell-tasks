myZip :: ([a],[b]) -> [(a,b)]
myZip ([], _) = []
myZip (_, []) = []
myZip (x:xs, y:ys) = (x, y) : (myZip (xs, ys))

combine :: (Ord a) => [a] -> [a] -> [a]
combine [] lst = lst
combine lst [] = lst
combine (x:xs) (y:ys) = 
	if x < y
		then x : combine xs (y:ys)
		else y : combine (x:xs) ys

sumSquares0 :: (Num a) => [a] -> a
sumSquares0 lst = 
	foldl (\x y -> (x + (y*y))) 0 lst

sumSquares1 :: (Num a) => [a] -> a
sumSquares1 [] = 0
sumSquares1 (x:xs) = x * x + (sumSquares1 xs)

sumSquares2 :: (Num a) => [a] -> a
sumSquares2 lst =
	let
		iter acc [] = acc
		iter acc (x:xs) = 
			iter (acc + x*x) xs
	in
		iter 0 lst

calcfunc :: Integer -> Double
calcfunc n =
	foldl (\x y -> x + (1 / (y * y))) 0 [1..m]
	where m = fromIntegral n

strangeCompare :: Integer -> Integer -> Ordering
strangeCompare a b = compare (final  a) (final b)

listify :: Integer -> [Integer]
listify 0 = [0]
listify n = (listify (n `div` 10)) ++ [n `mod` 10]

final :: Integer -> Integer
final n =
	foldl (\x y -> x*10 + y) 0 $ dropWhile (== 0) $ filter even $ listify n
