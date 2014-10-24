fact n = product[1..n]

fib n = head (drop n res)
	where res = 0 : 1 : zipWith (+) res (tail res)

f $$ x = f x
(f ... g) x = f (g x)

flip' a b c = a c b

gcd' a b = 
	if (y == 0) then x else 
	if (r == 0)
		then y
		else gcd' y (mod x y)
	where
	x = max (abs a) (abs b)
	y = min (abs a) (abs b)
	r = mod x y

isPrime a = if a < 2 then False
	else all (\x -> ((mod a x) /= 0)) [2..(div a 2)]

{--
sign a = if a > 0 then 1
	else if a < 0 then -1
		else 0

f :: Double -> Double
f x = x*x - 2*x + 1

linear :: Double -> Double
linear x = 3 * x - 17

root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps = 
	if (abs (f point)) < eps then point
		else if (sign (f a)) /= (sign (f point)) then
			(root f a point eps)
			else if (sign (f b)) /= (sign (f point)) then
				(root f point b eps)
				else if (abs (f firstSol)) < (abs (f secondSol)) then
					firstSol else secondSol
					where 
					point = (a + b) / 2
					firstSol = (root f a point eps)
					secondSol = (root f point b eps)
--}

myZip lst1 lst2 = 
	if (null lst1) || (null lst2) then [] else
		[(head lst1), (head lst2)] : (myZip (tail lst1) (tail lst2))

digitize num = 
	if (num < 10) then [num] else
		(digitize (div num 10)) ++ [(mod num 10)]

numerify lst =
	foldl (\x y -> (10 * x + y)) 0 lst

ord [] lst = lst
ord lst [] = lst
ord lst1 lst2 = 
	if (head lst1) < (head lst2) then
		(head lst1) : ord (tail lst1) lst2 else
			(head lst2) : ord lst1 (tail lst2)

sumSquares0 lst = 
	foldl (\x y -> (x + (y*y))) 0 lst

sumSquares1 lst = 
	if null lst then 0
		else (head lst) * (head lst) + (sumSquares1 (tail lst))


calcfunc n = sum [1 / (x * x) | x <- [1..n]]
