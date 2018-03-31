{-
    Edict.hs, E-dictionary and T9 input method with Trie and Patricia.
    Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import qualified Data.List
import qualified Trie

-- find all candidates in Trie
findAll:: Trie.Trie Char v -> String -> [(String, v)]
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

mapAppend x = map (\p->(x:(fst p), snd p))


-- T9 mapping
mapT9 = [('2', "abc"), ('3', "def"), ('4', "ghi"), ('5', "jkl"),
         ('6', "mno"), ('7', "pqrs"), ('8', "tuv"), ('9', "wxyz")]

lookupT9 :: Char -> [(Char, b)] -> [(Char, b)]
lookupT9 c children = case lookup c mapT9 of
        Nothing -> []
        Just s  -> foldl f [] s where
             f lst x = case lookup x children of
                 Nothing -> lst
                 Just t  -> (x, t):lst

-- T9-find in Trie
findT9:: Trie.Trie Char v -> String -> [(String, Maybe v)]
findT9 t [] = [("", Trie.value t)]
findT9 t (k:ks) = foldl f [] (lookupT9 k (Trie.children t))
    where
      f lst (c, tr) = (mapAppend c (findT9 tr ks)) ++ lst

toT9 = map (\c -> head $ [ d |(d, s) <- mapT9, c `elem` s])

-- test
testFindAll = "t=" ++ (show t) ++
              "\nlook up a: " ++ (show $ take 5 $findAll t "a") ++
              "\nlook up ab: " ++ (show $ take 5 $findAll t "ab") ++ "\n\n"
    where
      t = Trie.fromList lst
      lst=[("a", "the first letter of English"),
           ("an", "used instead of 'a' when the following word begins with a vowel sound"),
           ("another", "one more person or thing or an extra amount"),
           ("abandon", "to leave a place, thing or person forever"),
           ("about", "on the subject of; connected with"),
           ("adam", "a character in the Bible who was the first man made by God"),
           ("boy", "a male child or, more generally, a male of any age"),
           ("bodyl", "the whole physical structure that forms a person or animal"),
           ("zoo", "an area in which animals, especially wild animals, are kept so that people can go and look at them, or study them")]

testFindT9 = "t=" ++ (show t) ++
             "\npress 4: " ++ (show $ take 5 $ findT9 t "4")++
             "\npress 46: " ++ (show $ take 5 $ findT9 t "46")++
             "\npress 4663: " ++ (show $ take 5 $ findT9 t "4663")++
             "\npress 2: " ++ (show $ take 5 $ findT9 t "2")++
             "\npress 22: " ++ (show $ take 5 $ findT9 t "22")
    where
      t = Trie.fromList lst
      lst = [("home", 1), ("good", 2), ("gone", 3), ("hood", 4), ("a", 5), ("another", 6), ("an", 7)]

main = do
    putStrLn testFindAll
    putStrLn testFindT9
