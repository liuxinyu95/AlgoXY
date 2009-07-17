data Tree a = Empty 
            | Node (Tree a) a (Tree a) deriving (Show)

-- Helper to create a leaf
leaf::a -> Tree a
leaf a = Node Empty a Empty


-- Search in tree
search::(Ord a)=> Tree a -> a -> Tree a
search Empty _ = Empty
search (Node left y right) x | x == y = Node left y right
                             | x <  y = search left x
                             | x >  y = search right x

-- Tree Min
mint::Tree a -> Tree a
mint (Node Empty x right) = (Node Empty x right)
mint (Node left  _ _) = mint left

-- Tree Max
maxt::Tree a -> Tree a
maxt (Node left x Empty) = (Node left x Empty)
maxt (Node _ x right) = maxt right

-- successor
succ::Tree a -> Tree a
succ Empty = Empty
succ (Node _ x right) = mint right

-- predecessor

-- Insert an element into a tree
insert::(Ord a) => Tree a -> a -> Tree a
insert Empty x = leaf x
insert (Node left y right) x = if x < y 
                               then (Node (insert left x) y right)
                               else (Node left y (insert right x))

-- Helper to build a binary search tree from a list
listToTree::(Ord a)=>[a] -> Tree a
listToTree lst = foldl (\t x -> insert t x) Empty lst

--test data
t1 = leaf 4
t2 = listToTree [15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9]

main = do
  putStrLn $ show t1
  putStrLn ("test min: " ++ show (mint t1))
  putStrLn ("test listToTree: " ++ show t2)
  putStrLn ("test min: " ++ show (mint t2))
  putStrLn ("test search: " ++ show (search t2 18))