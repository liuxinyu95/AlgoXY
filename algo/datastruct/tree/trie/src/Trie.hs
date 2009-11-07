-- Refer to http://en.wikipedia.org/wiki/Trie

-- definition
data Trie a = Empty
            | Trie { value :: Maybe a,
                     children :: [(Char, Trie a)]}

-- insert
insert :: Trie a -> String -> a -> Trie a
insert t [] x = Trie (Just x) (children t)
insert t (k:ks) x 
    = case t of
        Empty -> Trie Nothing [(k, insert Empty ks x)]
        otherwise -> Trie (value t) (ins (children t) k ks x) where
             ins [] k ks x = [(k, (insert Empty ks x))]
             ins (e:es) k ks x = 

-- lookup
find :: Trie a -> String -> Maybe a
find t [] = value t
find Empty _ = Nothing
find t (k:ks) = case lookup k (children t) of
                  Nothing -> Nothing
                  Just t' -> find t' ks

{--
toString :: (show a)=> Trie a -> String
toString t = toStr t "" where
    toStr Empty prefix = "(" ++ (show prefix) ++ ")"
    toStr t prefix = "(" ++ (show prefix) ++ ":" + (show (value t)) ++
                     foldl 
--}

foo = Trie (Just 1) []

--main = do
--  insert t [] 5
--  putStrLn (show (find [] t))