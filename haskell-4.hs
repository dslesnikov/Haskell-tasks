data Suit = Hearts | Diamonds | Clubs | Spades
	deriving (Show, Read, Eq)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Show, Read, Eq, Ord)
data Card = Card
	{rank::Rank, suit::Suit }
	deriving (Show)

court :: Card -> Bool
court c = rank c >= Jack

isMinor :: Card -> Bool
isMinor = not . court

sameSuit :: Card -> Card -> Bool
sameSuit c1 c2 = (==) (suit c1) (suit c2)

beats :: Card -> Card -> Bool
beats c1 c2 = (rank c1) > (rank c2) && (sameSuit c1 c2)

beats2 :: Card -> Card -> Suit -> Bool
beats2 (Card rank1 suit1) (Card rank2 suit2) trump = 
	if suit1 == trump
		then if suit2 == trump
			then rank1 > rank2
			else True
		else if suit2 == trump
			then False
			else rank1 > rank2 && suit1 == suit2

beatsList :: [Card] -> Card -> Suit -> [Card]
beatsList cardlist c1 trump = filter (\x -> beats2 x c1 trump) cardlist

points1 :: Rank -> Int
points1 rank = 
	if court (Card rank Spades) && rank /= Ace
		then 10
		else
			if rank == Two
				then 2
				else if rank == Three
					then 3
					else if rank == Four
						then 4
						else if rank == Five
							then 5
							else if rank == Six
								then 6
								else if rank == Seven
									then 7
									else if rank == Eight
										then 8
										else if rank == Nine
											then 9
											else 10
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
