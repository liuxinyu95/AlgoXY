module Main where

import qualified Data.List
import qualified Trie
import Patricia

-- find all candidates in Trie
findAll:: Trie.Trie a -> String -> [(String, a)]
findAll t [] = 
    case Trie.value t of
      Nothing -> enum (Trie.children t) 
      Just x  -> ("", x):(enum (Trie.children t))
    where
      enum [] = []
      enum (p:ps) = (mapAppend (fst p) (findAll (snd p) [])) ++ (enum ps)
findAll t (k:ks) = 
    case lookup k (Trie.children t) of
      Nothing -> []
      Just t' -> mapAppend k (findAll t' ks)

mapAppend x lst = map (\p->(x:(fst p), snd p)) lst

-- find all candidates in Patricia
findAll' :: Patricia a -> Key -> [(Key, a)]
findAll' t [] =
    case value t of
      Nothing -> enum $ children t
      Just x  -> ("", x):(enum $ children t)
    where
      enum [] = []
      enum (p:ps) = (mapAppend' (fst p) (findAll' (snd p) [])) ++ (enum ps)
findAll' t k = find' (children t) k where
    find' [] _ = []
    find' (p:ps) k
          | (fst p) == k 
              = mapAppend' k (findAll' (snd p) [])
          | (fst p) `Data.List.isPrefixOf` k 
              = mapAppend' (fst p) (findAll' (snd p) (k `diff` (fst p)))
          | k `Data.List.isPrefixOf` (fst p) 
              = findAll' (snd p) []
          | otherwise = find' ps k
    diff x y = drop (length y) x

mapAppend' s lst = map (\p->(s++(fst p), snd p)) lst

-- T9-find in Trie

-- T9-find in Patricia

-- test
testFindAll = "t=" ++ (Trie.toString t) ++ 
              "\nlook up a: " ++ (show $ take 5 $findAll t "a") ++
              "\nlook up ab: " ++ (show $ take 5 $findAll t "ab") ++ "\n\n" ++
              "t'=" ++ (toString t') ++
              "\nlook up a: " ++ (show $ take 5 $findAll' t' "a") ++
              "\nlook up ab: " ++ (show $ take 5 $findAll' t' "ab")
    where 
      t = Trie.fromList lst
      t'= fromList lst
      lst=[("a", "the first letter of English"), 
           ("an", "used instead of 'a' when the following word begins with a vowel sound"), 
           ("another", "one more person or thing or an extra amount"), 
           ("abandon", "to leave a place, thing or person forever"),
           ("about", "on the subject of; connected with"),
           ("adam", "a character in the Bible who was the first man made by God"),
           ("boy", "a male child or, more generally, a male of any age"), 
           ("bodyl", "the whole physical structure that forms a person or animal"), 
           ("zoo", "an area in which animals, especially wild animals, are kept so that people can go and look at them, or study them")]

main = do
    putStrLn testFindAll