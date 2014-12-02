import Data.List

data Point = Point {x::Double, y::Double}
	deriving (Eq, Ord)
instance Show Point where
	show p = "(" ++ show (x p) ++ "; " ++ show (y p) ++ ")"

sump :: Point -> Point -> Point
sump p1 p2 = Point (x p1 + x p2) (y p1 + y p2)
subtrp :: Point -> Point -> Point
subtrp p1 p2 = Point (x p1 - x p2) (y p1 - y p2)

data Region =	Recktangle {vertice1::Point, vertice2::Point} |
				Circle {center::Point, radius::Double} |
				Union [Region] |
				Intersection [Region]

contains :: Region -> Point -> Bool
contains (Recktangle p1 p2) p = 
	(x p1 <= x p) && (x p <= x p2) &&
	(y p1 <= y p) && (y p <= y p2)
contains (Circle cen rad) p = 
	(sqrt (((x cen) - (x p))*((x cen) - (x p)) + ((y cen) - (y p))*((y cen) - (y p)))) < rad
contains (Union regs) p = 
	any (\x -> contains x p) regs
contains (Intersection regs) p =
	all (\x -> contains x p) regs

