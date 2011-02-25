{-
    KMP.hs, Knuth-Morris-Pratt string matching algorithm
    Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)

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

-- Usage:
--  search "aab" "abaabbabaaba"
--    > ("abaab", "babaaba")
--  search "aab" "babaaba"
--    > ("babaab", "a")
--  search "aab" "a"
--    > ("", "a")
search :: (Eq a) => [a] -> [a] -> ([a], [a])
search pattern text = searchKMP fallback [] pattern [], text
    where
      fallback = build pattern

type Fallback a = [a] -> [a] -> [a] -> [a] -> ([a], [a], [a], [a])

-- searchKMP sp ps sx xs
--   where pattern = reverse sp ++ ps
--         text    = reverse xs ++ xs
serachKMP :: (Eq a) => Fallback a -> [a] -> [a] -> [a] -> [a] -> ([a], [a])
searchKMP _ _ _  sx [] = ([], reverse sx) -- failed by the end
searchKMP _ _ [] sx xs = (reverse sx, xs) -- find a match
searchKMP f sp (p:ps) sx (x:xs) =
    if p == x then searchKMP (p:sp) ps (x:sx) xs
    else searchKMP sp' ps' sx' xs' where
        (sp', ps' sx' xs') = f sp (p:ps) sx (x:xs)

-- build fall back function
build :: (Eq a) => [a] -> Fallback a


-- example fallback
-- fallback [] "aab" xs (y:ys) = ([], "aab", (y:xs), ys) : move ahead
-- fallback "a" "ab" xs (y:ys) = ([], "aab", (y:xs), ys) : move ahead
-- fallback "aa" "b" xs (y:ys) = ("a", "ab", xs, (y:ys)) : stay here