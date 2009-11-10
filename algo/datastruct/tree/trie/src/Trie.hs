-- Refer to http://en.wikipedia.org/wiki/Trie

-- definition
data Trie a = Empty
            | Trie { value :: Maybe a
                   , children :: [(Char, Trie a)]}

-- insert
-- usage: insert t "zoo" "a place where animals are for public to see"
insert :: Trie a -> String -> a -> Trie a
insert Empty []     x = Trie (Just x) []
insert Empty (k:ks) x = Trie Nothing [(k, insert Empty ks x)]
insert t []     x = Trie (Just x)  (children t)
insert t (k:ks) x = Trie (value t) (ins (children t) k ks x) where
    ins [] k ks x = [(k, (insert Empty ks x))]
    ins (p:ps) k ks x = if fst p == k 
                        then (k, insert (snd p) ks x):ps
                        else p:(ins ps k ks x)

-- lookup
find :: Trie a -> String -> Maybe a
find t [] = value t
find Empty _ = Nothing
find t (k:ks) = case lookup k (children t) of
                  Nothing -> Nothing
                  Just t' -> find t' ks

fromList :: [(String, a)] -> Trie a
fromList xs = foldl ins Empty xs where
    ins t (k, v) = insert t k v

toString :: (Show a)=> Trie a -> String
toString t = toStr t "" where
    toStr :: (Show a) => Trie a -> String -> String
    toStr Empty prefix = "(" ++ prefix ++ ")"
    toStr t prefix = "(" ++ prefix ++ ":" ++ (showMaybe (value t)) ++
                     (concat $ map (\(k, v)->(toStr v prefix++[k])++ ",") (children t)) ++
                     "), "
    showMaybe Nothing = ""
    showMaybe (Just x)  = show x

testTrie = "t=" ++ (toString t) 
    where t = fromList [("a", 1), ("an", 2), ("another", 7), ("boy", 3), ("bool", 4), ("zoo", 3)]

main = do
  putStrLn testTrie