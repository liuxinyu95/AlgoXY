{-
 NthElem.hs
 Copyright (C) 2011 Liu Xinyu (liuxinyu95@gmail.com)
 
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
module NthElem where

import Data.List
import Test.QuickCheck

-- the k-selection problem.

tops _ [] = []
tops 0 _  = []
tops n (x:xs) | len ==n = as
              | len < n  = as ++ [x] ++ tops (n-len-1) bs
              | otherwise = tops n as
    where
      (as, bs) = partition (<= x) xs
      len = length as
      
top n (x:xs) | len == n - 1 = x
             | len < n - 1 = top (n - len - 1) bs
             | otherwise = top n as
    where                           
      (as, bs) = partition (<=x) xs
      len = length as

prop_tops :: Int ->[Int] ->Bool
prop_tops k xs = if k>=0 then sort (tops k xs) == take k (sort xs)
                else True
                     
prop_top :: Int ->[Int] -> Property
prop_top k xs = xs /= [] ==> let k' = (abs(k) `mod` (length xs)) + 1 in sort (xs) !! (k'-1) == top k' xs