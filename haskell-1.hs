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
		else if (sign (f a)) /= (sign (f point)) then
			(root f a point eps)
			else (root f point b eps)
	where
		point = (a + b) / 2
		sign a = if a > 0 then 1
			else if a < 0 then -1
				else 0
