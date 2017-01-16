BEISPIEL 1

> data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq,Ord,Show)
> data Order = Up | Down deriving (Eq,Show)

leerer baum

> nil :: Tree a
> nil = Nil

ist nil?

> isNilTree :: (Eq a) => Tree a -> Bool
> isNilTree x = (x == nil)

ist nicht nil?

> isNodeTree :: (Eq a) => Tree a -> Bool
> isNodeTree x = (x /= nil)

gibt linken teilbaum zurück

> leftSubTree :: Tree a -> Tree a
> leftSubTree Nil = error "Empty Tree as Argument"
> leftSubTree (Node a left right) = left

gibt rechten teilbaum zurück

> rightSubTree :: Tree a -> Tree a
> rightSubTree Nil = error "Empty Tree as Argument"
> rightSubTree (Node a left right) = right

gibt den wert des root nodes

> treeValue :: Tree a -> a
> treeValue (Nil) = error "Empty Tree as Argument"
> treeValue (Node a left right) = a

gibt true, falls wert "val" im baum ist

> isValueOf :: Eq a => a -> Tree a -> Bool
> isValueOf _ Nil = False
> isValueOf val tree
>	| val == treeValue tree = True
>	| True == (isValueOf val (leftSubTree(tree))) = True
>	| True == (isValueOf val (rightSubTree(tree))) = True
>	| otherwise = False

gibt true, falls baum geordnet ist (linke teilbäume immer kleiner, rechte immer größer)

> isOrderedTree :: Ord a => Tree a -> Bool
> isOrderedTree tree
>	| (leftNotNil tree) && ((treeValue (leftSubTree tree)) > (treeValue tree) )= False
>	| (rightNotNil tree) && ((treeValue (rightSubTree tree)) < (treeValue tree)) = False
>	| (leftNotNil tree) && ((isOrderedTree.leftSubTree $ tree) == False) = False
>	| (rightNotNil tree) && ((isOrderedTree.rightSubTree $ tree) == False) = False
>	| otherwise = True

gibt true, falls links nicht nil

> leftNotNil :: Eq a =>  Tree a -> Bool
> leftNotNil Nil = False
> leftNotNil ( Node _ l _) = (False == isNilTree l)

gibt true, falls rechts nicht nil

> rightNotNil :: Eq a => Tree a -> Bool
> rightNotNil Nil = False
> rightNotNil ( Node _ _ r)  = (False == isNilTree r)

fügt val entsprechend in den baum ein

> insert :: Ord a => a -> Tree a -> Tree a
> insert val Nil = (Node val Nil Nil)
> insert val ( Node nod l r)
>	| (isOrderedTree ( Node nod l r)) == False = error "Argument Tree not Ordered"
>	| otherwise = insertNode val ( Node nod l r)

> insertNode :: Ord a => a -> Tree a -> Tree a
> insertNode val Nil = (Node val Nil Nil)
> insertNode val ( Node nod l r)
>	| (val == nod) = (Node nod l r)
>	| (val < nod) = (Node nod (insertNode val l) r)
>	| (val > nod) = (Node nod l (insertNode val r))

delegiert löschen aufruf falls baum geordnet ist

> delete :: Ord a => a -> Tree a -> Tree a
> delete val Nil = Nil
> delete val (Node nod l r)
>	| (isOrderedTree (Node nod l r)) == False = error "Argument Tree not Ordered"
>	| otherwise = deleteNode val (Node nod l r)

löscht val entsprechend aus dem baum, findet entsprechenden Node

> deleteNode :: Ord a => a -> Tree a -> Tree a
> deleteNode _ Nil = Nil
> deleteNode val ( Node nod l r)
>	| (val == nod) = deleteNodeCases (Node nod l r)
>	| (val < nod) = (Node nod (deleteNode val l) r)
>	| (val > nod) = (Node nod l (deleteNode val r))

fallunterscheidung: welche anhängenden bäume gibt es beim zu löschenden Knoten?

> deleteNodeCases :: (Ord a) => Tree a -> Tree a
> deleteNodeCases (Node nod Nil r) = r
> deleteNodeCases (Node nod l Nil) = l
> deleteNodeCases (Node nod l r) = (Node nod2 l r)
>	where
>		nod2 = getNewRoot r

get smallest element of right subtree for reconfiguring tree and make it ordered again

> getNewRoot :: (Ord a) => Tree a -> a
> getNewRoot (Node nod Nil _) = nod
> getNewRoot (Node _ l _) = getNewRoot l

macht einen tree in eine geordnete liste, auf oder absteigend

> flatten :: Ord a => Order -> Tree a -> [a]
> flatten order tree
>	| (isOrderedTree tree) == False = error "Argument Tree not Ordered"
>	| order == Up = sortedList tree
>	| order == Down = reverse (sortedList tree )

fügt rekursiv an liste werte von linkem subbaum nach links bzw. rechtem subbaum nach rechts

> sortedList :: (Ord a) => Tree a -> [a]
> sortedList Nil = []
> sortedList (Node a t1 t2) = sortedList t1 ++ [a] ++ sortedList t2

BEISPIEL 2

> maxLength :: Ord a => Tree a -> Int
> maxLength Nil = 0
> maxLength tree = getTreeLength tree 0


get max int from: left and right sub tree, iterate further

> getTreeLength :: Ord a => Tree a -> Int -> Int
> getTreeLength Nil curr = curr
> getTreeLength (Node a t1 t2) curr = getMax  (getTreeLength t1 (curr + 1)) (getTreeLength t2 (curr + 1)) 


> getMax :: Int -> Int -> Int
> getMax x y
>	| x > y = x
>	| otherwise = y


> getMin :: Int -> Int -> Int
> getMin x y
>	| x < y = x
>	| otherwise = y


> minLength :: Ord a => Tree a -> Int
> minLength Nil = 0
> minLength tree = getTreeLengthMin tree 0

> getTreeLengthMin :: Ord a => Tree a -> Int -> Int
> getTreeLengthMin Nil curr = curr
> getTreeLengthMin (Node a t1 t2) curr = getMin  (getTreeLength t1 (curr + 1)) (getTreeLength t2 (curr + 1)) 


> balancedDegree :: Ord a => Tree a -> Int
> balancedDegree tree = ((maxLength tree) - (minLength tree))


Trees zum austesten: 

> unordered :: Tree Char  
> unordered =   
>    Node 'P'  
>        (Node 'O'  
>            (Node 'L'  
>                (Node 'N' Nil Nil)  
>                (Node 'T' Nil Nil)  
>            )  
>            (Node 'Y'  
>                (Node 'S' Nil Nil)  
>                (Node 'A' Nil Nil)  
>            )  
>        )  
>        (Node 'L'  
>            (Node 'W'  
>                (Node 'C' Nil Nil)  
>                (Node 'R' Nil Nil)  
>            )  
>            (Node 'A'  
>                (Node 'A' Nil Nil)  
>                (Node 'C' Nil Nil)  
>            )  
>        )

> ordered :: Tree Char  
> ordered =   
>    Node 'P'  
>        (Node 'F'  
>            (Node 'C'  
>                (Node 'A' Nil Nil)  
>                (Node 'D' Nil Nil)  
>            )  
>            (Node 'H'  
>                (Node 'E' Nil Nil)  
>                (Node 'Y' Nil Nil)  
>            )  
>        )  
>        (Node 'V'  
>            (Node 'T'  
>                (Node 'A' Nil Nil)  
>                (Node 'Z' Nil Nil)  
>            )  
>            (Node 'X'  
>                (Node 'A' Nil Nil)  
>                (Node 'Z' Nil Nil)  
>            )  
>        )

> small :: Tree Char
> small =
>	Node 'F'  
>            (Node 'C'  
>                (Node 'A' Nil Nil)  
>                (Node 'D' Nil Nil)  
>            )  
>            (Node 'H'  
>                (Node 'E' Nil Nil)  
>                (Node 'Y' Nil Nil)  
>            ) 

> maxMinTree :: Tree Char
> maxMinTree =
>	Node 'F'  
>            (Node 'C'  
>                (Node 'C'  
>                	(Node 'C'  
>                (Node 'A' Nil Nil)  
>                (Node 'D' Nil Nil) )  
>                	(Node 'D' Nil Nil) )  
>                (Node 'D' Nil Nil)  
>            )  
>            (Node 'H' Nil Nil 
>            ) 
