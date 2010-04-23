{-
    STreeStr.hs, String manipulation with Suffix Tree in Haskell.
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

module STreeStr where

import Stree
import Data.Function (on)

biggest::(a->a->)->[a]->[a]

-- Search the longest repeated substring
lrs::STree->String
lrs Lf _ = ""
lrs BR lst = find lst where
    find [] = ""
    find ((s, t):xs) = maximumBy (compare `on` length) (s ++ lrs t):(find xs)