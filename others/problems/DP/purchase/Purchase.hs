module Purchase where

import qualified Data.Map as Map
import Data.Set

-- Problem: Given multiple product discount plans,
--   for e.g. sell products A, B, C together at price X
-- For a wish list of product type [M, N, ...] find the cheapest purchase plan
--
-- For example, with the below plans:

plan1 = Map.fromList [("AB",  100)
                     ,("BCD", 150)
                     ,("AD",  125)
                     ,("CD",  135)]

-- Below are the cheapest purchase plan:
-- lowest cost of "BAD"  # ["AB", "AD"]  ==> 225
-- lowest cost of "BAC"  # ["AB", "CD"]  ==> 235
-- lowest cost of "BCD"  # ["BCD"] ==> 150

-- Here is another example. We use 0-9A-Z to enumerate products:

plan2 = Map.fromList [("816309", 11)
                     ,("7824", 28)
                     ,("487i620", 47)
                     ,("649", 57)
                     ,("407396812", 57)
                     ,("986750123", 64)
                     ,("9642", 86)
                     ,("16480579", 107)
                     ,("9648350", 111)
                     ,("8937514", 128)]

-- the optimal purchase for product list "704938521" is
-- ["986750123", "7824"] ==> 28 + 64 = 92

type DpTab = Map.Map Integer (Set String)  -- Map cost (Set product)

-- Dynamic Programming solution
--  Build the DP table
dpTable :: Map.Map String Integer -> DpTab
dpTable = Map.foldrWithKey build (Map.singleton 0 empty)

build :: String -> Integer -> DpTab -> DpTab
build pkg price tab = Map.foldrWithKey expand tab tab where
  expand cost prods tab = let cost' = cost + price in
    case Map.lookup cost' tab of
      Nothing -> Map.insert cost' (insert pkg prods) tab
      Just prods' -> if (fromList pkg) `isSubsetOf` (unionAll prods') then tab
        else Map.insert cost' (insert pkg prods') tab

unionAll :: Set String -> Set Char
unionAll = Data.Set.foldr (\str set -> (fromList str) `union` set) empty

lowest :: Set Char -> DpTab -> Maybe (Integer, Set String)
lowest wish tab = Map.lookupMin $ Map.filter ((wish `isSubsetOf`) . unionAll) tab

-- examples
tab1 = dpTable plan1

-- lowest (fromList "BAD") tab1
--       Just (225,fromList ["AB","AD"])
-- lowest (fromList "BAC") tab1
--       Just (235,fromList ["AB","CD"])
-- lowest (fromList "BCD") tab1
--       Just (150,fromList ["BCD"])

test1 = Prelude.map (\s -> lowest (fromList s) tab1) ["BAD", "BAC", "BCD"]

tab2 = dpTable plan2

test2 = lowest (fromList "704938521") tab2 -- contains err here
