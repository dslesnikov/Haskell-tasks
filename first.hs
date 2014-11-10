gcd' 0 a = a
gcd' a 0 = a
gcd' a b = gcd' b (mod a b)

isPrime a = if a < 2 then False
	else all (\x -> ((mod a x) /= 0)) [2..(div a 2)]


rev num = 
	foldl (\ x y -> 10*x + y) 0  (listify num)
	where
		listify num = 
			if num < 10 then [num] else
				(mod num 10) : (listify (div num 10))


square_eq_root a b c = 
	let disc = b * b - 4 * a * c
	in
		if disc < 0
			then error "Square equation has no roots"
			else (max ((-b + sqrt(disc)) / (2 * a)) ((-b - sqrt(disc)) / (2 * a)))

f :: Double -> Double
f x = x*x - 2*x 

linear :: Double -> Double
linear x = 3 * x - 17

root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps = 
	if (abs (f point)) < eps then point
		else if (signum (f a)) /= (signum (f point)) then
			(root f a point eps)
			else (root f point b eps)
	where point = (a + b) / 2
