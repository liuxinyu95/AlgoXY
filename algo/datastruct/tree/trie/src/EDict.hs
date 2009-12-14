module Main where

import Trie

-- find all candidates
findAll:: Trie a -> String -> [(String, a)]
findAll t [] = 
    case value t of
      Nothing -> enum (children t) 
      Just x  -> ("", x):(enum (children t))
    where
      enum [] = []
      enum (p:ps) = (mapAppend (fst p) (findAll (snd p) [])) ++ (enum ps)
findAll t (k:ks) = 
    case lookup k (children t) of
      Nothing -> []
      Just t' -> mapAppend k (findAll t' ks)

mapAppend x lst = map (\p->(x:(fst p), (snd p))) lst

-- test
testFindAll = "t=" ++ (toString t) ++ 
              "\nlook up a: " ++ (show $ take 5 $findAll t "a") ++
              "\nlook up ab: " ++ (show $ take 5 $findAll t "ab")
    where 
      t = fromList [("a", "the first letter of English"), 
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