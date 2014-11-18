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
listifyAll n = concatMap (\x -> listify x) [1..n]

listify :: Int -> [Int]
listify 0 = []
listify n = reverse $ (n `mod` 10) : (listify $ n `div` 10)


--		#6
triangl :: [Int]
triangl = 1 : zipWith (+) [2..] triangl

triangles :: Int -> [Int]
triangles n = take n triangl
