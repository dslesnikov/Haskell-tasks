data Tree a = Empty | Node a (Tree a) (Tree a) 

instance Show a => Show (Tree a) where
	show tree = "\n" ++ show' tree 0 

show' :: Show a => Tree a -> Int -> String 
show' Empty indent = replicate (3*indent) ' ' ++ "Empty"
show' (Node el left right) indent = show' left (indent + 1) ++ "\n"
	++ replicate (3*indent) ' ' ++ show el ++ "\n"
	++ show' right (indent + 1)


addElem :: Ord a => Tree a -> a -> Tree a
addElem Empty elem = Node elem Empty Empty
addElem tree@(Node root lb rb) elem =
	if elem < root
		then (Node root (addElem lb elem) rb)
		else if elem == root
				then tree
				else (Node root lb (addElem rb elem))

testTree = foldl addElem Empty [8, 4, 5, 6, 18, 15, 9, 20]


contains :: Ord a => Tree a -> a -> Bool
contains Empty _ = False
contains (Node root lb rb) elem = 
	if root == elem
		then True
		else if elem < root
			then contains lb elem
			else contains rb elem



count :: Tree a -> Int
count Empty = 0
count (Node root lb rb) = 1 + count lb + count rb


sumElems :: Num a => Tree a -> a
sumElems Empty = 0
sumElems (Node root lb rb) = root + sumElems lb + sumElems rb

prodElems :: Num a => Tree a -> a
prodElems Empty = 1
prodElems (Node root lb rb) = root * prodElems lb * prodElems rb


findTheRightest :: Tree a -> a
findTheRightest (Node root _ Empty) = root
findTheRightest (Node root _ rb) = findTheRightest rb

delElem :: Ord a => Tree a -> a -> Tree a
delElem Empty _ = Empty
delElem (Node root Empty rb) elem =
	if elem == root
		then rb
		else Node root Empty (delElem rb elem)
delElem (Node root lb Empty) elem =
	if elem == root
		then lb
		else Node root (delElem lb elem) Empty
delElem tree@(Node root lb rb) elem = 
	if not $ contains tree elem
		then tree
		else if root == elem
			then Node (findTheRightest lb) (delElem lb (findTheRightest lb)) rb
			else if elem < root
				then Node root (delElem lb elem) rb
				else Node root lb (delElem rb elem)
