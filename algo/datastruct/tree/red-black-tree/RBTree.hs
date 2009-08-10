data Color = R | B deriving Show
data RBTree a = Empty
              | Node Color (RBTree a) a (RBTree a)

-- Helper functions
-- pretty printing
instance Show a => Show (RBTree a) where
    show Empty = "()"
    show (Node c l k r) = "(" ++ show l ++ ", " ++ 
                          show k ++ ":" ++ show c ++ ", " ++ 
                          show r ++ ")"

-- Insertion/Deletion

leaf::a -> RBTree a
leaf a = Node R Empty a Empty

insert::(Ord a)=>RBTree a -> a -> RBTree a
insert t x = makeBlack(ins t x) where  --[1]
    ins Empty x = Node R Empty x Empty --[2]
    ins (Node color l key r) x = if x < key
                                 then balance color (ins l x) key r
                                 else balance color l key (ins r x) --[3]
    makeBlack(Node _ l key r) = Node B l key r

{- footnotes
[1]: Always set the root color as black;
[2]: new node is inserted as a red leaf, 
     to reserve property 1, 3, 5, only property 2, 4 may be violated
[3]: All keys should be indentical -}

balance::Color -> RBTree a -> a -> RBTree a ->RBTree a

balance color l key r = Node color l key r

-- helper function to build a red black tree from a list

listToRBTree::(Ord a)=>[a] -> RBTree a
listToRBTree lst = foldl insert Empty lst

t1=listToRBTree [11, 2, 14, 1, 7, 15, 5, 8, 4]

main = do
  putStrLn $ show t1
