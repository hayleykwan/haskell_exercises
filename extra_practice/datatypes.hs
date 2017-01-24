data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
              deriving (Eq, Show)

tree1 = (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))) :: Tree Int
tree2 = (Node (Node Empty 'a' (Leaf 'e')) 'h' (Node (Node Empty 'k' (Node Empty 'l' (Leaf 'l'))) 's' Empty)) :: Tree Char

-- post: returns the number of values stored in the tree.
treeCount :: Tree a -> Int
treeCount (Empty) = 0
treeCount (Leaf a) = 1
treeCount (Node t1 a t2) = 1 + treeCount t1 + treeCount t2

-- post: returns a list of all the items stored in the tree.
flatten :: Tree a -> [a]
flatten (Empty) = []
flatten (Leaf a) = [a]
flatten (Node l a r) = flatten l ++ [a] ++ flatten r

-- post: returns the highest value stored in the tree.
highest :: Tree Int -> Int
highest (Empty) = 0
highest (Leaf a) = a
highest (Node l a r) = maximum (flatten (Node l a r)) 

-- post: returns a new tree with every item stored multiplied by the given amount.
multiply :: Int -> Tree Int -> Tree Int
multiply n (Empty) = Empty
multiply n (Leaf a) = Leaf (a*n)
multiply n (Node l a r) = Node (multiply n l) (a*n) (multiply n r)

-- post: returns a new tree with every item in both trees added pairwise.
addTrees :: Tree Int -> Tree Int -> Tree Int
-- pre: given trees have same structures
addTrees Empty Empty = Empty
addTrees (Leaf a) (Leaf b) = Leaf (a+b)
addTrees (Node l a r) (Node l' a' r') 
  = Node (addTrees l l') (a+a') (addTrees r r')




data List a = Nil | Cons a (List a)
            deriving (Eq, Ord, Show)

-- post: returns the length of the given list.
myLength :: List a -> Int
myLength Nil = 0
myLength (Cons a x)
  | x == Nil = 1
  | otherwise = 1 + myLength x  

-- post: applies the given function to every element of the input list,
--       returning a list for the value returned for each application.
myMap :: List a -> (a -> b) -> List b
myMap Nil function = Nil
myMap (Cons a x) 







