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

module KMP where

import Data.List
import Data.Function (on)

kmpSearch pattern text = kmpSearch' next ([], pattern) ([], text)

kmpSearch' _ (sp, []) (sw, []) = [length sw]
kmpSearch' _ _ (_, []) = []
kmpSearch' f (sp, []) (sw, ws) = length sw : kmpSearch' f (f sp []) (sw, ws)
kmpSearch' f (sp, (p:ps)) (sw, (w:ws))
    | p == w = kmpSearch' f ((p:sp), ps) ((w:sw), ws)
    | otherwise = kmpSearch' f (f sp ps) (sw, (w:ws))
                  
next [] ps = ([], ps)
next [p] ps = ([], p:ps)
next sp ps = (sp', ps') where
    prev = reverse sp
    prefix = longest [xs | xs <- init $ tail $ inits prev,
                           xs `isSuffixOf` prev]
    sp' = reverse prefix
    ps' = (prev ++ ps) \\ prefix
    longest xs = if xs==[] then [] 
                 else maximumBy (compare `on` length) xs

