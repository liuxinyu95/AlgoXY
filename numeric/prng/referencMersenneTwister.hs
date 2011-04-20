-- ^ A Haskell library for MT19937, based on the C code
-- Takuji Nishimura and Makoto Matsumoto.
-- Translated to Haskell by Lennart Augustsson during ICFP 2006.
module MersenneTwister(mersenneTwister) where
import Control.Monad.ST.Strict as S
import Control.Monad.ST.Lazy as L
import Data.Word(Word32)
import Data.Bits
import Data.Array.ST

type W = Word32

n, m :: Int
n = 624
m = 397

matrix_a, upper_mask, lower_mask :: W
matrix_a = 0x9908b0df		    -- constant vector a
upper_mask = 0x80000000		    -- most significant w-r bits
lower_mask = 0x7fffffff		    -- least significant r bits

type MT s = STUArray s Int W

init_genrand :: W -> S.ST s (MT s)
init_genrand s = do
    mt <- newArray_ (0,n-1)
    writeArray mt 0 s
    let set mti = do
            o <- readArray mt (mti-1)
	    let v = 1812433253 * (o `xor` (o `shiftR` 30)) + fromIntegral mti
	    writeArray mt mti v
    mapM set [1..n-1]
    return mt

genrand_int32 :: Int -> MT s -> S.ST s (W, Int)
genrand_int32 mti' mt = do
    mti <- if mti' >= n then do step mt; return 0 else return mti'
    y0 <- readArray mt mti
    let y1 = y0 `xor` (y0 `shiftR` 11)
    	y2 = y1 `xor` ((y1 `shiftL`  7) .&. 0x9d2c5680)
        y3 = y2 `xor` ((y2 `shiftL` 15) .&. 0xefc60000)
        y4 = y3 `xor` (y3 `shiftR` 18)
    return (y4, mti+1)

step :: MT s -> S.ST s ()
step mt = do
    let mag01 x = if odd x then matrix_a else 0
        set kk = do
	    mtkk  <- readArray mt kk
	    mtkk1 <- readArray mt ((kk+1) `mod` n)
	    mtkkm <- readArray mt ((kk+m) `mod` n)
            let y = (mtkk .&. upper_mask) .|. (mtkk1 .&. lower_mask)
	        v = mtkkm `xor` (y `shiftR` 1) `xor` mag01 y
	    writeArray mt kk v
    mapM_ set [0 .. n-1]

genRands :: Int -> MT s -> L.ST s [W]
genRands mti mt = do
    (y, mti') <- strictToLazyST $ genrand_int32 mti mt
    ys <- genRands mti' mt
    return (y : ys)

mersenneTwisterST :: W -> L.ST s [W]
mersenneTwisterST s = do
    mt <- strictToLazyST $ init_genrand s
    genRands n mt

mersenneTwister :: W -> [W]
mersenneTwister s = L.runST (mersenneTwisterST s)
