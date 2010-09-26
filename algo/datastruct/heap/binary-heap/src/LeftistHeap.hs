{-
    LeftistHeap.hs, Leftist Heap in Haskell
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

module LeftistHeap where

-- Definition

data LHeap a = E -- Empty 
             | Node{ r :: Int -- rank
                   , elem :: a   -- element
                   , left :: LHeap
                   , right :: LHeap } deriving (Eq, Show)

merge::(Ord a)=>LHeap a -> LHeap a -> LHeap a
merge E h = h
merge h E = h
merge h1@(Node _ x l r) h2@(Node _ y l' r') = 
    if x < y then makeNode x l merge r h2
    else makeNode y l' merge h1 r'

makeNode:: a -> LHeap a -> LHeap a -> LHeap a
makeNode x a b = if rank a < rank b then Node 