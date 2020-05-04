module Purchase where

import Data.Map as Map
import Data.Set as Set

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

type Plan = Map String Integer  -- Map package price
type Packages = Set String

-- Different Packages may cost same, hence we use Set Packages.

type DpTab = Map Integer (Set Packages)  -- Map cost (Set Packages)

-- Dynamic Programming solution
--  Build the DP table
dpTable :: Plan -> DpTab
dpTable = foldrWithKey build (Map.singleton 0 Set.empty)

build :: String -> Integer -> DpTab -> DpTab
build pkg price tab = foldrWithKey expand tab tab where
  expand cost pkgss tab = if Set.null pkgss' then tab
                          else insertWith Set.union cost' pkgss' tab where
    cost' = cost + price
    pkgss' = pkg `exInsert` pkgss

-- Exclusive Insert, for each set of packages combination, only insert the
-- packages when it can't be covered
exInsert :: String -> Set Packages -> Set Packages
exInsert pkg pkgss
  | Set.null pkgss = Set.singleton (Set.singleton pkg)
  | otherwise = Set.map (Set.insert pkg) $ Set.filter (not . exists pkg) pkgss

-- if a package can be fully covered by a set of packages combination
exists :: String -> Packages -> Bool
exists pkg pkgs = (Set.fromList pkg) `isSubsetOf` (Set.unions $ Set.map Set.fromList pkgs)

-- Lookup the DP table for the lowest cost purchase plans (multiple results)
lowest :: String -> DpTab -> Maybe (Integer, Set Packages)
lowest wish = filterPkg . Map.lookupMin . (Map.filter (any (exists wish))) where
  filterPkg Nothing = Nothing
  filterPkg (Just (cost, pkgss)) = Just (cost, Set.filter (exists wish) pkgss)

-- examples
tab1 = dpTable plan1
test1 = Prelude.map (\s -> lowest s tab1) ["BAD", "BAC", "BCD"]

tab2 = dpTable plan2
test2 = lowest "704938521" tab2
