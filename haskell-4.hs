data Suit = Hearts | Diamonds | Clubs | Spades
	deriving (Show, Eq)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Show, Eq, Ord)
data Card = Card
	{ rank::Rank, suit::Suit }
	deriving (Show, Eq)

court :: Card -> Bool
court c = rank c >= Jack

isMinor :: Card -> Bool
isMinor = not . court

sameSuit :: Card -> Card -> Bool
sameSuit c1 c2 = (==) (suit c1) (suit c2)

beats :: Card -> Card -> Bool
beats c1 c2 = (rank c1) > (rank c2) && (sameSuit c1 c2)

beats2 :: Card -> Card -> Suit -> Bool
beats2 c1@(Card rank1 suit1) c2@(Card rank2 suit2) trump = 
	if suit1 == trump
		then if suit2 == trump
			then rank1 > rank2
			else True
		else if suit2 == trump
			then False
			else beats c1 c2

beatsList :: [Card] -> Card -> Suit -> [Card]
beatsList cardlist c1 trump = filter (\x -> beats2 x c1 trump) cardlist

points1 Two = 2
points1 Three = 3
points1 Four = 4
points1 Five = 5
points1 Six = 6
points1 Seven = 7
points1 Eight = 8
points1 Nine = 9
points1 Ten = 10
points1 Jack = 10
points1 Queen = 10
points1 King = 10

points :: Card -> Int
points c = points1 $ rank c

acePoints :: [Card] -> [Int]
acePoints aces = take (n + 1) $ iterate (+ 10) n
	where n = length aces

listPoints :: [Card] -> [Int]
listPoints cards =
	let pointsForNotAces = foldl (\acc el -> acc + (points el)) 0 $ filter (\x -> rank x /= Ace) cards
	in
		map (+ pointsForNotAces) $ acePoints $ filter (\x -> rank x == Ace) cards

{--
*Main> listPoints [(Card Ace Spades), (Card Ace Diamonds), (Card King Hearts), (
Card Nine Clubs)]
[21,31,41]
--}
