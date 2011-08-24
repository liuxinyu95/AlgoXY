{-
    RAList.hs, Random Access List
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

module RAList where

import Test.QuickCheck

-- Based on Chris Okasaki's ``Purely Functional Datastructures''

-- Different with Okasaki's original version, we denote leaf as 
-- Node 1 x Empty Empty, so that the definition of binary tree
-- is same as the normal binary tree with size augmented.

-- Binary tree representation

data Tree a = Empty 
            | Node Int (Tree a) (Tree a) -- size, left, right

-- Numeric representation, Binary numer
data Digit a = Zero
             | One (Tree a)

-- The random access list is represented as a forest of binary trees.
newtype RAList a = [Digit a]

-- Auxilary functions
size :: Tree a -> Int
size Empty = 0
size (Node sz _ _) = sz

-- Precondition: rank t1 = rank t2
link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

cons :: a -> RAList a -> RAList a
cons x ts = insertTree ts (Leaf 1 x 0 0) 

insertTree :: RAList a -> Tree a -> RAList a
insertTree [] t = [One t]
insertTree (Zero:ts) t = One t : ts
insertTree (One t' :ts)= Zero : insertTree (link t t') ts


-- testing

-- Reference
-- [1]. Chris Okasaki. ``Purely Functional Random-Access Lists''. Functional Programming Languages and Computer Architecutre, June 1995, pages 86-95.
