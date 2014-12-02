import Data.List

data Point = Point { x::Double, y::Double }
	deriving(Eq, Ord)
instance Show Point where
	show p = "(" ++ show (x p) ++ "; " ++ show (y p) ++ ")"

vector :: Point -> Point -> Point
vector p1 p2 = Point ((-) (x p1) (x p2)) ((-) (y p1) (y p2))

wedgeProduct :: Point -> Point -> Double
wedgeProduct p1 p2 = (-) ((x p1) * (y p2)) ((x p2) * (y p1))

norm :: Point -> Point -> Double
norm p1 p2 = sqrt ((+) ((x $ (vector p1 p2))*(x $ (vector p1 p2))) ((y $ (vector p1 p2))*(y $ (vector p1 p2))))

base :: [Point] -> Point
base points = minimumBy (\point1 point2 -> compare (Point (y point1) (x point1)) (Point (y point2) (x point2))) points


findLeastByPolarArg :: [Point] -> Point -> Point -> Point
findLeastByPolarArg points base current = 
	minimumBy	(\point1 point2 -> compare	(wedgeProduct (vector current base) (vector point1 base))
											(wedgeProduct (vector current base) (vector point2 base)))
				points

decision :: Point -> Point -> Point -> Point
decision least base current =
	if product <= 0
		then least
		else current
	where
		product = wedgeProduct (vector current base) (vector least current)


jarvis :: [Point] -> [Point]
jarvis points = 
	b : map
	where
