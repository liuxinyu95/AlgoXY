module LeapFrog2D where

import qualified Data.Map as M
import Data.Sequence (singleton, ViewL(..), viewl, null, (|>))
import Data.Bits
import Data.Ix (inRange)
import Prelude hiding (null)

-- Problem reference:
-- Mathematical puzzles of Sam Loyd, selected and edited by Martin Gardner. 1959 by Dover Publications, Inc.
-- Chinese translation:
-- Chen Weipeng,
-- Shanghai scientific & technological education publish house. 1999
-- ISBN 7-5428-1928-3

-- board layout:

-- B, B, B
-- B, B, B
-- B, B, *, W, W
--       W, W, W
--       W, W, W

-- The board is combined by two 3x3 squares. There are 16 pieces (8 black and 8 white)
-- and an empty cell.
-- where B is black piece, W is white piece, * is empty cell.
-- rules: leap or hop to exchange all the blacks and whites
-- the movement can only happen either vertically or horizontally, but not in diagonal direction.

-- BFS search
-- Input a queue and the visited board layout history
--   The element in queue is a board layout, a board layout is a pair of 2 parts:
--     One is the position (i,j) indicating where is the free cell;
--     The other is a binary number represents the board.
--   The history is a map from layout n to layout n'.
--     Where n' is the parent state of n, (n can be achieved from some movement from n')

start = bin $ concat [[1,1,1,0,0],
                      [1,1,1,0,0],
                      [1,1,0,0,0],
                      [0,0,0,0,0],
                      [0,0,0,0,0]]

end = bin $ concat [[0,0,0,0,0],
                    [0,0,0,0,0],
                    [0,0,0,1,1],
                    [0,0,1,1,1],
                    [0,0,1,1,1]]

bin = foldl1 (\n d -> 2*n + d)

ans = solve (singleton s0) (M.singleton s0 s0) where
  solve q h | null q = []
            | otherwise = let (c@(p, n) :< q') = viewl q in
    if n == end then backtrack c h []
    else let (cs', h') = move p n h in solve (foldl (|>) q' cs') h'
  s0 = ((3, 3), start)

move p n h = (cs, foldr (\c h' -> M.insert c (p, n) h') h cs) where
  cs = [(p', n') |
        d <- [(0, 1), (1, 0), (0, -1), (-1, 0), (0, 2), (2, 0), (0, -2), (-2, 0)],
        let p' = p `offset` d,
        inBoard p', getAt n p' == (- signum (fst d + snd d)),
        let n' = norm p' $ swapbits n p p', (p', n') `M.notMember` h]
  offset (x, y) (dx, dy) = (x + dx, y + dy)

backtrack c h cs = let c' = h M.! c in if c' == c then (c:cs) else backtrack c' h (c:cs)

setbit n i = n .|. (1 `shiftL` i)

clrbit n i = n .&. (complement (1 `shiftL` i))

getbit n i = (n `shiftR` i) .&. 1

inBoard p = inRange ((1, 1), (3, 3)) p || inRange ((3, 3), (5, 5)) p

idx (i, j) = 25 - 5 * (i-1) - j

swapbits n p p' = setAt (setAt n d m') d' m
  where
    (m, m') = (idx p, idx p')
    (d, d') = (getbit n m, getbit n m')

setAt n 1 m = setbit n m
setAt n 0 m = clrbit n m

getAt n p = 2 * (getbit n (idx p)) - 1

norm p n = clrbit n (idx p)

toStr (p, n) = unlines [[piece n p (i, j) | i <-[1..5]] | j <-[1..5]] where
  piece n p p' | p == p' = '*' -- the free cell
               | not $ inBoard p' = ' '
               | getAt n p' == 1 = 'W'
               | otherwise = 'B'

output = do
  mapM_ (putStrLn . toStr) ans
  putStrLn $ "total " ++ (show $ length ans) ++ " steps"
