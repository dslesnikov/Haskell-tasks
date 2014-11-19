import Data.Char
--		#1
aLotOfSins :: Int -> Double
aLotOfSins n = (iterate sin 1) !! n

--		#2
doubleEvens :: (Integral a) => [a] -> [a]
doubleEvens lst = concatMap (\x -> if even x then [x, x] else [x]) lst

--		#4
leastN :: Integer
leastN = floor $ head $ dropWhile (\x -> sin (x*x) < 0.9999) [1..]

--		#5
listifyAll :: Int -> [Int]
listifyAll n = map digitToInt $ concatMap show [1..n]

--		#6
triangl :: [Int]
triangl = 1 : zipWith (+) [2..] triangl

triangles :: Int -> [Int]
triangles n = take n triangl

--		#7
almNewton :: Int -> [Int]
almNewton 0 = 1 : (repeat 0)
almNewton n = zipWith (+) (0 : prev) prev
	where
		prev = almNewton (n-1)
newton :: Int -> [Int]
newton n = takeWhile (/= 0) (almNewton n)
