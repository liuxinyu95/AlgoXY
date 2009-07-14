data Tree a = Empty 
            | Node (Tree a) a (Tree a) deriving (Show)

-- Helper to create a leaf
leaf::a -> Tree a
leaf a = Node Empty a Empty

-- Tree Min
mint::Tree a -> a
mint (Node Empty x _) = x
mint (Node left  _ _) = mint left

-- Insert an element into a tree
insert::(Ord a) => Tree a -> a -> Tree a
insert Empty x = leaf x
insert (Node left y right) x = if x < y 
                               then (Node (insert left x) y right)
                               else (Node left y (insert right x))

-- Helper to build a binary search tree from a list
buildTree::(Ord a)=>[a] -> Tree a
buildTree [] = Empty
buildTree (x:xs) = insert (buildTree xs) x

--test data
t1 = leaf 4
t2 = buildTree [15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9]

main = do
  putStrLn $ show t1
  putStrLn $ show (mint t1)
  putStrLn $ show t2
  putStrLn $ show (mint t2)