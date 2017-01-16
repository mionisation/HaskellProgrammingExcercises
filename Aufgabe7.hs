--Aufagbe 7

-- ZAHLWERT IST ANZAHL DER VORKOMMEN ????
import Data.List
data Tree a = Nil | Node a Int (Tree a) (Tree a) deriving (Eq,Ord,Show)
type Multiset a = Tree a
data ThreeValuedBool = TT | FF | Invalid deriving (Eq,Show)
data Order = Up | Down deriving (Eq,Show)

-- 1 --
-- true falls tree ein multiset ist
-- gibt true, falls baum geordnet ist (linke teilbäume immer kleiner, rechte immer größer)
-- und int >= 0

isMultiset :: Ord a => Tree a -> Bool
isMultiset Nil = True
isMultiset tree
	| ((treeInt tree) < 0) ||  ( (leftNotNil tree) && ((treeValue (leftSubTree tree)) > (treeValue tree) ) ) = False
	| ((treeInt tree) < 0) || ( (rightNotNil tree) && ((treeValue (rightSubTree tree)) < (treeValue tree)) ) = False
	| ((treeInt tree) < 0) || ( (leftNotNil tree) && ((isMultiset.leftSubTree $ tree) == False) ) = False
	| ((treeInt tree) < 0) || ( (rightNotNil tree) && ((isMultiset.rightSubTree $ tree) == False) ) = False
	| otherwise = True

treeInt :: Tree a -> Int
treeInt (Nil) = error "Empty Tree as Argument"
treeInt (Node a  i left right) = i

-- gibt den wert des root nodes

treeValue :: Tree a -> a
treeValue (Nil) = error "Empty Tree as Argument"
treeValue (Node a  i left right) = a


-- gibt linken teilbaum zurück

leftSubTree :: Tree a -> Tree a
leftSubTree Nil = error "Empty Tree as Argument"
leftSubTree (Node a i left right) = left

-- gibt rechten teilbaum zurück

rightSubTree :: Tree a -> Tree a
rightSubTree Nil = error "Empty Tree as Argument"
rightSubTree (Node a i left right) = right


-- gibt true, falls links nicht nil

leftNotNil :: Eq a =>  Tree a -> Bool
leftNotNil Nil = False
leftNotNil ( Node _ _ l _) = (False == isNilTree l)

-- gibt true, falls rechts nicht nil

rightNotNil :: Eq a => Tree a -> Bool
rightNotNil Nil = False
rightNotNil ( Node _ _ _ r)  = (False == (isNilTree r))


-- 2 --
-- true falls tree ein canonical multiset ist, selbe wie in 1, aber int > 0
isCanonicalMultiset :: Ord a => Tree a -> Bool
isCanonicalMultiset Nil = True
isCanonicalMultiset tree
	| ((treeInt tree) <= 0) ||  ( (leftNotNil tree) && ((treeValue (leftSubTree tree)) > (treeValue tree) ) ) = False
	| ((treeInt tree) <= 0) || ( (rightNotNil tree) && ((treeValue (rightSubTree tree)) < (treeValue tree)) ) = False
	| ((treeInt tree) <= 0) || ( (leftNotNil tree) && ((isCanonicalMultiset.leftSubTree $ tree) == False) ) = False
	| ((treeInt tree) <= 0) || ( (rightNotNil tree) && ((isCanonicalMultiset.rightSubTree $ tree) == False) ) = False
	| otherwise = True

{-

-- 3 -- 
-- make new search tree out of tree: 
-- iterate old tree, insert new values, integer is sum of integers of same value in the old tree or 0
-}

-- todo, could make faster
-- get set of values of old tree and build new tree from it
mkMultiset :: (Ord a,Show a) => Tree a -> Multiset a
mkMultiset oldTree = insertListItemsAndValues Nil oldTree (nub.sortedList$oldTree)

-- iterates the set and uses getSumOfVals to get value for each entry, build new tree from it
insertListItemsAndValues :: (Ord a,Show a) => Tree a -> Tree a -> [a] -> Tree a
insertListItemsAndValues newTree oldTree [] = newTree
insertListItemsAndValues newTree oldTree (x:xs) = insertListItemsAndValues (insertv x (max 0 (getSumOfVals oldTree x)) newTree) oldTree xs

-- gives back the sum of integer value associated with VAL in TREE
getSumOfVals :: (Eq a, Ord a,Show a) => Tree a -> a -> Int
getSumOfVals Nil _ = 0
getSumOfVals tree val
	| val == treeValue tree = (getSumOfVals (leftSubTree(tree)) val ) + (treeInt tree) + (getSumOfVals (rightSubTree(tree)) val )
	| otherwise = (getSumOfVals (leftSubTree(tree)) val ) + (getSumOfVals (rightSubTree(tree)) val )

-- fügt rekursiv an liste werte von linkem subbaum nach links bzw. rechtem subbaum nach rechts

sortedList :: (Ord a) => Tree a -> [a]
sortedList Nil = []
sortedList (Node a _ t1 t2) = [a] ++ sortedList t1 ++ sortedList t2


-- fügt val entsprechend in den baum ein

insertv :: Ord a => a -> Int -> Tree a -> Tree a
insertv val int Nil = (Node val int Nil Nil)
insertv val int ( Node nod int2 l r)
	| (isMultiset ( Node nod int2 l r)) == False = error "Argument Tree not Ordered"
	| otherwise = insertNode val int ( Node nod int2 l r)

insertNode :: Ord a => a -> Int -> Tree a -> Tree a
insertNode val int Nil = (Node val int Nil Nil)
insertNode val int ( Node nod  int2 l r)
	| (val == nod) = (Node nod int2 l r)
	| (val < nod) = (Node nod int2 (insertNode val int l) r)
	| (val > nod) = (Node nod int2 l (insertNode val int r))


-- 4 --

mkCanonicalMultiset :: (Ord a,Show a) => Tree a -> Multiset a
mkCanonicalMultiset oldTree = caninsertListItemsAndValues Nil oldTree (nub.sortedList$oldTree)

-- iterates the set and uses getSumOfVals to get value for each entry, build new tree from it
caninsertListItemsAndValues :: (Ord a,Show a) => Tree a -> Tree a -> [a] -> Tree a
caninsertListItemsAndValues newTree oldTree [] = newTree
caninsertListItemsAndValues newTree oldTree (x:xs) = caninsertListItemsAndValues (insertv x (max 1 (getSumOfVals oldTree x)) newTree) oldTree xs


-- 5 --
-- done on multiset, ordered list of nodes with > 0 (Up or Down), 
flatten :: (Ord a,Show a) => Order -> Multiset a -> [(a,Int)]
flatten order multiset
	| (isMultiset multiset) == False = []
	| order == Up = sortedList2 multiset
	| order == Down = reverse (sortedList2 multiset)

sortedList2 :: (Ord a) => Tree a -> [(a, Int)]
sortedList2 Nil = []
sortedList2 (Node a i t1 t2)
	| i == 0 = sortedList2 t1 ++ sortedList2 t2
	| otherwise = sortedList2 t1 ++ [(a, i)] ++ sortedList2 t2

-- 6 --

isElement :: Ord a => a -> Multiset a -> Int
isElement char multiset 
	| (isMultiset multiset) == False = -1


-- gives back the amount of occurences of a in multiset
getNumOfVals :: (Eq a, Ord a,Show a) => Tree a -> a -> Int
getNumOfVals Nil _ = 0
getNumOfVals tree val
	| val == treeValue tree = (getNumOfVals (leftSubTree(tree)) val ) + 1 + (getNumOfVals (rightSubTree(tree)) val )
	| otherwise = (getNumOfVals (leftSubTree(tree)) val ) + (getNumOfVals (rightSubTree(tree)) val )

-- 7 --
-- TT if first multiset a subset of second multiset: if every value in first appears at least as often in the second 
--isSubset :: Ord a => Multiset a -> Multiset a -> ThreeValuedBool


{-
-- gibt true, falls wert "val" im baum ist

isValueOf :: Eq a => a -> Tree a -> Bool
isValueOf _ Nil = False
isValueOf val tree
	| val == treeValue tree = True
	| True == (isValueOf val (leftSubTree(tree))) = True
	| True == (isValueOf val (rightSubTree(tree))) = True
	| otherwise = False




-- delegiert löschen aufruf falls baum geordnet ist

delete :: Ord a => a -> Tree a -> Tree a
delete val Nil = Nil
delete val (Node nod l r)
	| (isOrderedTree (Node nod l r)) == False = error "Argument Tree not Ordered"
	| otherwise = deleteNode val (Node nod l r)

-- löscht val entsprechend aus dem baum, findet entsprechenden Node

deleteNode :: Ord a => a -> Tree a -> Tree a
deleteNode _ Nil = Nil
deleteNode val ( Node nod l r)
	| (val == nod) = deleteNodeCases (Node nod l r)
	| (val < nod) = (Node nod (deleteNode val l) r)
	| (val > nod) = (Node nod l (deleteNode val r))

-- fallunterscheidung: welche anhängenden bäume gibt es beim zu löschenden Knoten?

deleteNodeCases :: (Ord a) => Tree a -> Tree a
deleteNodeCases (Node nod Nil r) = r
deleteNodeCases (Node nod l Nil) = l
deleteNodeCases (Node nod l r) = (Node nod2 l r)
	where
		nod2 = getNewRoot r

-- get smallest element of right subtree for reconfiguring tree and make it ordered again

getNewRoot :: (Ord a) => Tree a -> a
getNewRoot (Node nod Nil _) = nod
getNewRoot (Node _ l _) = getNewRoot l


-- BEISPIEL 2

maxLength :: Ord a => Tree a -> Int
maxLength Nil = 0
maxLength tree = getTreeLength tree 0


-- get max int from: left and right sub tree, iterate further

getTreeLength :: Ord a => Tree a -> Int -> Int
getTreeLength Nil curr = curr
getTreeLength (Node a t1 t2) curr = getMax  (getTreeLength t1 (curr + 1)) (getTreeLength t2 (curr + 1)) 


getMax :: Int -> Int -> Int
getMax x y
	| x > y = x
	| otherwise = y


getMin :: Int -> Int -> Int
getMin x y
	| x < y = x
	| otherwise = y


minLength :: Ord a => Tree a -> Int
minLength Nil = 0
minLength tree = getTreeLengthMin tree 0

getTreeLengthMin :: Ord a => Tree a -> Int -> Int
getTreeLengthMin Nil curr = curr
getTreeLengthMin (Node a t1 t2) curr = getMin  (getTreeLength t1 (curr + 1)) (getTreeLength t2 (curr + 1)) 


balancedDegree :: Ord a => Tree a -> Int
balancedDegree tree = ((maxLength tree) - (minLength tree))

-}

--Alte Files von Aufgabe 4:
--leerer baum

nil :: Tree a
nil = Nil

-- ist nil?

isNilTree :: (Eq a) => Tree a -> Bool
isNilTree x = (x == nil)

-- ist nicht nil?

isNodeTree :: (Eq a) => Tree a -> Bool
isNodeTree x = (x /= nil)



-- Trees zum austesten: 

unorderedCanMult :: Tree Char  
unorderedCanMult =   
    Node 'P'  1
        (Node 'O'  2
            (Node 'L'  3
                (Node 'N' 4 Nil Nil)  
                (Node 'T' 3 Nil Nil)  
            )  
            (Node 'Y' 4 
                (Node 'S' 2 Nil Nil)  
                (Node 'A' 2 Nil Nil)  
            )  
        )  
        (Node 'L'  2
            (Node 'W'  2
                (Node 'C' 1 Nil Nil)  
                (Node 'R' 1 Nil Nil)  
            )  
            (Node 'A' 3 
                (Node 'A' 3 Nil Nil)  
                (Node 'C' 2 Nil Nil)  
            )  
        )

orderedCanMulti :: Tree Char  
orderedCanMulti =   
    Node 'P' (2) 
        (Node 'F' 3 
            (Node 'C'  1
                (Node 'A' 2 Nil Nil)  
                (Node 'D' 3 Nil Nil)  
            )  
            (Node 'H'  8
                (Node 'E' 2 Nil Nil)  
                (Node 'Y' 2 Nil Nil)  
            )  
        )  
        (Node 'V'  5
            (Node 'T'  3
                (Node 'A' 2 Nil Nil)  
                (Node 'Z' 1 Nil Nil)  
            )  
            (Node 'X'  2
                (Node 'A' 4 Nil Nil)  
                (Node 'Z' 3 Nil Nil)  
            )  
        )
smallUnCanMulti :: Tree Char
smallUnCanMulti =
	Node 'F'  2
            (Node 'C' 0 
                (Node 'A' 6 Nil Nil)  
                (Node 'D' 3 Nil Nil)  
           )  
            (Node 'H'  9
                (Node 'E' 2 Nil Nil)  
                (Node 'Y' 1 Nil Nil)  
            ) 

maxMinTreeCanMulti :: Tree Char
maxMinTreeCanMulti =
	Node 'F'  3
            (Node 'C' 2 
                (Node 'C' 2 
                	(Node 'C'  5
                (Node 'A' 3  Nil Nil)  
               (Node 'D' 5 Nil Nil) )  
                	(Node 'D' 8 Nil Nil) )  
                (Node 'D' 9 Nil Nil)  
            )  
            (Node 'H' 2 Nil Nil 
            ) 
