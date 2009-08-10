-- RedBlackTree.hs

module RedBlackTree (RedBlackTree, rbempty, rbisempty, rbinsert, rbmember, rbdelete, rbupdate, rblookup, rbdeleteat,
					rbtolist, rbfromlist, rbfromlist', rbcount, rbmap, rbany, rball, rbcountwith, rbmin, rbmax, toSet,
					rbkeys, rbvalues, rbaddupdate, rbinc, rbdec) where

-- Based on Okasaki, Purely Functional Data Structures
-- as extended by Stefan Kahrs to add delete (from http://www.cs.kent.ac.uk/people/staff/smk/redblack/untyped.hs)

data Color = R | B deriving (Show, Read)
data RedBlackTree a = E | T Color (RedBlackTree a) a (RedBlackTree a) deriving (Show, Read)

instance Eq a => Eq (RedBlackTree a) where
	t1 == t2 = rbtolist t1 == rbtolist t2

instance Ord a => Ord (RedBlackTree a) where
	compare t1 t2 = compare (rbtolist t1) (rbtolist t2)
-- in other words, Eq and Ord for RedBlackTree is based on comparison of the elements, irrespective of the tree structure

-- red black trees work by maintaining two invariants (plus the order invariant left < node < right)
-- 1. A red node may not have a red parent
-- 2. Starting at the root, every path to a leaf has the same number of black nodes

rbempty :: RedBlackTree a
rbempty = E

rbisempty :: RedBlackTree a -> Bool
rbisempty E = True
rbisempty _ = False


-- SETS - INSERTION AND MEMBERSHIP

-- insertion and membership test by Okasaki
rbinsert :: Ord a => RedBlackTree a -> a -> RedBlackTree a
rbinsert s x =
	T B a z b
	where
	T _ a z b = ins s
	ins E = T R E x E -- the new node is inserted as a red leaf, thus preserving the black invariant
	ins s@(T B a y b) = -- the calls to balance fix the red invariant while maintaining the black invariant
		case compare x y of
		LT -> balance (ins a) y b
		GT -> balance a y (ins b)
		EQ -> s
	ins s@(T R a y b) = -- any red violations arising from this insertion are fixed by the parent black node
		case compare x y of
		LT -> T R (ins a) y b
		GT -> T R a y (ins b)
		EQ -> s
-- the "black-depth" of the tree increases if balancing moves a red node to the root - it is recoloured to black

rbmember :: Ord a => a -> RedBlackTree a -> Bool
rbmember x E = False
rbmember x (T _ a y b) =
	case compare x y of
	LT -> rbmember x a
	GT -> rbmember x b
	EQ -> True

{- balance: first equation is new,
   to make it work with a weaker invariant -}
balance :: RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance a x b = T B a x b
-- balance a' x' b' returns a replacement for T B a' x' b'
-- this fixes red violations while maintaining the black invariant
-- the effect is always to move red nodes up the tree (towards the root), and also to reduce the number of red nodes


-- SETS - DELETION

-- deletion by Kahrs
rbdelete :: Ord a => RedBlackTree a -> a -> RedBlackTree a
rbdelete t x =
	case del t of {T _ a y b -> T B a y b; _ -> E}
	where
	del E = E
	del (T _ a y b) =
		case compare x y of
		LT -> delleft a y b
		GT -> delright a y b
		EQ -> append a b
	delleft a@(T B _ _ _) y b = balleft (del a) y b
	delleft a y b = T R (del a) y b
	delright a y b@(T B _ _ _) = balright a y (del b)
	delright a y b = T R a y (del b)

balleft :: RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
balleft (T R a x b) y c = T R (T B a x b) y c
balleft bl x (T B a y b) = balance bl x (T R a y b)
balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z (sub1 c))

balright :: RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
balright a x (T R b y c) = T R a x (T B b y c)
balright (T B a x b) y bl = balance (T R a x b) y bl
balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)

sub1 :: RedBlackTree a -> RedBlackTree a
sub1 (T B a x b) = T R a x b
sub1 _ = error "invariance violation"

-- append the left and right trees
-- we know the max of the left is smaller than the min of the right
-- and we know that they have the same black-depth
append :: RedBlackTree a -> RedBlackTree a -> RedBlackTree a
append E x = x -- x might be E, or it might be a red node
append x E = x
append (T R a x b) (T R c y d) =
	case append b c of
	    T R b' z c' -> T R(T R a x b') z (T R c' y d)
	    bc -> T R a x (T R bc y d)
append (T B a x b) (T B c y d) = 
	case append b c of
	    T R b' z c' -> T R(T B a x b') z (T B c' y d)
	    bc -> balleft a x (T B bc y d)
append a (T R b x c) = T R (append a b) x c
append (T R a x b) c = T R a x (append b c)


-- FINITE MAPS

rblookup :: Ord k => RedBlackTree (k,a) -> k -> Maybe a
rblookup E _ = Nothing
rblookup (T _ a (l,x) b) k =
	case compare k l of
	LT -> a `rblookup` k
	GT -> b `rblookup` k
	EQ -> Just x

rbupdate :: Ord k => RedBlackTree (k,a) -> (k,a) -> RedBlackTree (k,a)
rbupdate s (k,x) =
	let T _ a y b = dorbupdate s
	in T B a y b
	where
		dorbupdate E = T R E (k,x) E
		dorbupdate s@(T B a (l,y) b) =
			case compare k l of
			LT -> balance (dorbupdate a) (l,y) b
			GT -> balance a (l,y) (dorbupdate b)
			EQ -> T B a (k,x) b -- we overwrite the existing value
		dorbupdate s@(T R a (l,y) b) =
			case compare k l of
			LT -> T R (dorbupdate a) (l,y) b
			GT -> T R a (l,y) (dorbupdate b)
			EQ -> T R a (k,x) b -- we overwrite the existing value

rbdeleteat :: Ord k => RedBlackTree (k,a) -> k -> RedBlackTree (k,a)
rbdeleteat t k =
	case del t of {T _ a y b -> T B a y b; _ -> E}
	where
	del E = E
	del (T _ a y@(l,_) b) =
		case compare k l of
		LT -> delleft a y b
		GT -> delright a y b
		EQ -> append a b
	delleft a@(T B _ _ _) y b = balleft (del a) y b
	delleft a y b = T R (del a) y b
	delright a y b@(T B _ _ _) = balright a y (del b)
	delright a y b = T R a y (del b)


-- UTILITIES

rbtolist :: RedBlackTree a -> [a]
rbtolist E = []
rbtolist (T _ a y b) = rbtolist a ++ (y : rbtolist b)

rbfromlist :: Ord a => [a] -> RedBlackTree a
rbfromlist xs = foldl rbinsert E xs

rbfromlist' :: Ord k => [(k,v)] -> RedBlackTree (k,v)
rbfromlist' kvs = foldl rbupdate E kvs

rbcount :: RedBlackTree a -> Int
rbcount E = 0
rbcount (T _ a x b) = 1 + rbcount a + rbcount b

rbmap :: (Ord a, Ord b) => (a -> b) -> RedBlackTree a -> RedBlackTree b
rbmap f E = E
rbmap f (T colour a x b) = T colour (rbmap f a) (f x) (rbmap f b) 
-- NOTE: f had better be one-to-one and order-preserving, otherwise the resulting rbtree may not satisfy the order invariant


rbany :: (a -> Bool) -> RedBlackTree a -> Bool
rbany p E = False
rbany p (T _ a x b) = p x || rbany p a || rbany p b

rball :: (a -> Bool) -> RedBlackTree a -> Bool
rball p E = True
rball p (T _ a x b) = p x && rball p a && rball p b

rbcountwith :: (a -> Bool) -> RedBlackTree a -> Int
rbcountwith p E = 0
rbcountwith p (T _ a x b) = (if p x then 1 else 0) + (rbcountwith p a) + (rbcountwith p b)

rbmin (T _ E x _) = x 
rbmin (T _ t _ _) = rbmin t

rbmax (T _ _ x E) = x 
rbmax (T _ _ _ t) = rbmax t

toSet xs = rbtolist (rbfromlist xs)

rbkeys :: RedBlackTree (k,v) -> [k]
rbkeys E = []
rbkeys (T _ a (k,_) b) = rbkeys a ++ (k : rbkeys b)
-- == map fst (rbtolist t)

rbvalues :: RedBlackTree (k,v) -> [v]
rbvalues E = []
rbvalues (T _ a (_,v) b) = rbvalues a ++ (v : rbvalues b)
-- == map snd (rbtolist t)


rbaddupdate t (k,x) =
	case t `rblookup` k of
	Nothing -> t `rbinsert` (k,x)
	Just y -> t `rbupdate` (k,y+x)

rbinc t k = rbaddupdate t (k,1)

rbdec t k = rbaddupdate t (k,-1)


-- TESTS

redinvariant E = True
redinvariant (T R (T R _ _ _) _ _) = False
redinvariant (T R _ _ (T R _ _ _)) = False
redinvariant (T _ l _ r) = redinvariant l && redinvariant r

blackdepth E = 0 -- we don't count Es as real nodes - if they were, they would be black
blackdepth (T B a _ _) = 1 + blackdepth a
blackdepth (T R a _ _) = blackdepth a

blackinvariant E = True
blackinvariant (T _ a _ b) = blackdepth a == blackdepth b && blackinvariant a && blackinvariant b

orderinvariant E = True
orderinvariant (T _ E x E) = True
orderinvariant (T _ a x E) = rbmax a < x && orderinvariant a
orderinvariant (T _ E x b) = x < rbmin b && orderinvariant b
orderinvariant (T _ a x b) = rbmax a < x && x < rbmin b && orderinvariant a && orderinvariant b

rbinvariant t = redinvariant t && blackinvariant t && orderinvariant t