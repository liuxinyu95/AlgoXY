module LeapFrog2D where

import qualified Data.Map as M --(singleton, insert, (!), notMember)
import Data.Bits
import Data.Ix (inRange)
import Debug.Trace

-- BFS search
-- Input a queue and the visited board layout history
--   The element in queue is a board layout, a board layout is pair:
--     One is the position (i,j) indicating where is the free cell;
--     The other is a binary number represents the board.
--   The history is map from a layout n to layout n'.
--     Where n' is the parent state of n, (n can be achieved from some movement from n')

type Point = (Int, Int)
type Layout = (Point, Integer)
type History = M.Map Layout Layout

traceLog msg a = a --trace (msg ++ (show a)) a

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

s0 = ((3, 3), start)

ans = solve [s0] (M.singleton s0 s0) where
  solve :: [Layout] -> History -> [Layout]
  solve [] _ = []
  solve (c@(p, n):cs) h | n == end = backtrack c h []
                        | otherwise = let (cs', h') = move p n h in solve (cs ++ (traceLog "cs'=" cs')) h'

move :: Point -> Integer -> History -> ([Layout], History)
move p n h = (cs, foldr (\c h' -> M.insert c (p, n) h') h cs) where
  delta = concat [[(di, dj), (2*di, 2*dj)] | (di, dj) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]]
  cs = [(p', n') | d <- delta, let p' = p `offset` d,
        inBoard p', getAt n p' == (- signum (fst d + snd d)),
        let n' = norm p' $ swapbits n p p', (p', n') `M.notMember` h]

backtrack c h cs = let c' = h M.! c in if c' == c then (c:cs) else backtrack c' h (c:cs)

-- bitwise functions

bin = foldl1 (\n d -> 2*n + d)

toInt :: Integer -> Int
toInt = fromIntegral

getbit n i = toInt ((n `shiftR` i) .&. 1)
setbit n i = n .|. (1 `shiftL` i)
clrbit n i = n .&. (complement (1 `shiftL` i))

swapbits :: Integer -> Point -> Point -> Integer
swapbits n p p' = setAt (setAt n d m') d' m
  where
    (m, m') = (idx p, idx p')
    (d, d') = (getbit n m, getbit n m')

setAt n 1 m = setbit n m
setAt n 0 m = clrbit n m

getAt :: Integer -> Point -> Int
getAt n p = 2 * (getbit n (idx p)) - 1

-- board related

idx :: Point -> Int
idx (i, j) = 25 - 5 * (i-1) - j

norm p n = clrbit n (idx p)

offset (x, y) (dx, dy) = (x + dx, y + dy)

inBoard p = inRange s1 p || inRange s2 p where
  s1 = ((1, 1), (3, 3))
  s2 = ((1, 1) `offset` (2, 2), (3, 3) `offset` (2, 2))

toStr (p, n) = unlines [[piece n p (i, j) | i <-[1..5]] | j <-[1..5]] where
  piece n p p' | p == p' = '*' -- the free cell
               | not $ inBoard p' = ' '
               | getAt n p' == 1 = 'W'
               | otherwise = 'B'

output = do
  mapM_ (putStrLn . toStr) ans
  putStrLn $ "total " ++ (show $ length ans) ++ " steps" where
