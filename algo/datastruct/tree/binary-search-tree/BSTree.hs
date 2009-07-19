data Tree a = Empty 
            | Node (Tree a) a (Tree a) deriving (Show)

-- Helper functions for tree
leaf::a -> Tree a
leaf a = Node Empty a Empty

left::Tree a -> Tree a
left (Node l _ _) = l
left _ = Empty

right::Tree a -> Tree a
right (Node _ _ r) = r
right _ = Empty

key::Tree a -> a
key (Node _ k _) = k

isEmpty::Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- in-order tree walk
inOrderWalk::Tree a -> (a->b) -> Tree b
inOrderWalk Empty f = Empty
inOrderWalk t f = (Node (inOrderWalk (left t) f) (f (key t)) (inOrderWalk (right t) f))

-- Search in tree
search::(Ord a)=> Tree a -> a -> Tree a
search Empty _ = Empty
search t x | key(t)==x = t
           | x < key(t) = search (left t) x
           | otherwise = search (right t) x

-- Find parent of a value in tree
parent::(Ord a)=>Tree a -> a -> Tree a
parent Empty _ = Empty
parent t x | (key t) == x = Empty
           | key (left t) == x || key (right t) == x = t --Error: left or right may be empty
           | otherwise = if x < key t then parent (left t) x else parent (right t) x

-- Find an ancestor of a value wich match a certain rule
findAncestor::(Ord a)=>Tree a -> a ->(a->a->Bool)-> Tree a
findAncestor t x f = checkParent t (parent t x) x f where
    checkParent _ Empty _ _ = Empty
    checkParent t p x f = if f (key p) x then p
                          else checkParent t (parent t (key p)) x f


-- Tree Min
mint::Tree a -> Tree a
mint t = if isEmpty (left t) then t else mint $ left t

-- Tree Max
maxt::Tree a -> Tree a
maxt t = if isEmpty (right t) then t else maxt $ right t

-- successor
succt::(Ord a)=>Tree a -> a -> Tree a
succt Empty _ = Empty
succt t x = if not $ isEmpty rightNode
            then mint rightNode
            else findAncestor t x (>) where
                rightNode = right (search t x)

-- predecessor
predt::(Ord a)=>Tree a -> a -> Tree a
predt Empty _ = Empty
predt t x = if not $ isEmpty leftNode
            then maxt leftNode
            else findAncestor t x (<) where
                leftNode = left (search t x)
              
-- Insert an element into a tree
insert::(Ord a) => Tree a -> a -> Tree a
insert Empty x = leaf x
insert t x = if x < key(t) 
             then (Node (insert (left t) x) (key t) (right t))
             else (Node (left t) (key t) (insert (right t) x))

-- Helper to build a binary search tree from a list
listToTree::(Ord a)=>[a] -> Tree a
listToTree lst = foldl (\t x -> insert t x) Empty lst

-- test data
t1 = leaf 4
t2 = listToTree [15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9]

-- test tree creation
testBuildTree = "\ntest create empty:\t"++ show (Empty::Tree Int)++
                "\ntest create leaf:\t" ++ show t1 ++ 
                "\ntest create from list:\t" ++ show t2

-- test tree walk
testTreeWalk = "\ntest tree in-order walk by apply (-):\t"++show (inOrderWalk t2 (\x -> -x))

-- test min/max for tree
testMinMax = "\ntest min empty:\t" ++ (show $ mint (Empty::Tree Int)) ++
             "\ntest min leaf:\t" ++  show (mint t1) ++
             "\ntest min tree:\t" ++ show (mint t2) ++ 
             "\ntest max empty:\t" ++ (show $ maxt (Empty::Tree Int)) ++
             "\ntest max leaf:\t" ++ show (maxt t1) ++
             "\ntest max tree:\t" ++ show (maxt t2)

-- test search in tree
testSearch = "\ntest search empty tree:\t"++ show (search (Empty::Tree Int) 3) ++
             "\ntest search in leaf:\t"++ show (search t1 4)++
             "\ntest search non exist value in leaf:\t" ++ show (search t1 5)++
             "\ntest search non exist node in tree:\t"++ show (search t2 5)++
             "\ntest search a node in tree:\t"++ show (search t2 18)

-- test succ/pred
testSuccPred = "\ntest succ of 7:\t"++ show (succt t2 7) ++
               "\ntest parent of 7:\t"++ show (parent t2 7)++
               "\ntest parent of 13:\t"++ show (parent t2 13)++
               "\ntest bigger ancestor of 7:\t"++ show (findAncestor t2 7 (>))++
               "\ntest bigger ancestor of 13:\t"++ show (findAncestor t2 13 (>))
               --"\ntest succ of 13:\t"++ show (succt t2 13) ++
               --"\ntest pred of 6:\t"++ show (predt t2 6)++
               --"\ntest rped of 7:\t"++ show (predt t2 7)

main = do
  putStrLn testBuildTree
  putStrLn testMinMax
  putStrLn testSearch
  putStrLn testTreeWalk
  putStrLn testSuccPred