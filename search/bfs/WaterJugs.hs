-- General solution to 2 jugs:

-- Problem: 
-- Given 2 jugs of volumn a and b (a < b), bring up g volumns of water from the river.

-- Constraint:
-- g must be value of n * gcd(a, b), written as gcd(a, b) | g
-- otherwise, it's unsolvable.
-- a and b are not neccessarily relatively prime, for gcd(a, b) = 1, it's possible to
-- bring up any quantity of g.

-- Memo of extended Euclid algorithm:
-- The extended Euclide algorithm does not give GCD of a and b, but also give one of
-- the linear combination so that 

--     a x + b y = gcd(a, b)

-- Define this function as:

--     (d, x, y) = ext-gcd(a, b) 

-- Without loss of genericity, assume a < b. there exists quotation q and remainder r that

--    b = a q + r

-- Since d = gcd(a, b), it can divide both a and b, thus d can divide r as well.
-- Because r is less than a, we can scale down the problem to find the GCD of a and r:

--    (d, x', y') = ext-gcd(r, a)

-- Where d = x' r + y' a according to the definition of extended Euclid algorithm.

-- Transform b = a q + r to r = b - a q, sustitude r in above equation yeilds:

--    d = x' (b - a q) + y' a   this can be further transform to:
--    d = (y' - x' q) a + x' b

-- This is linear combination of a and b, so that we have:

--    x = y' - x' (b / d)
--    y = x'

-- Beside this recursive case, the edge case happens when a = 0:
--  gcd(a, b) = b = 0 a + 1 b

-- Summarize the above gives the extended Euclide algorithm:
extGcd 0 b = (b, 0, 1)
extGcd a b = (d, y' - x' * (b / d), x') where (d, x', y') = extGcd(b `mod` a, a)

-- Method 1, Extended Euclid algorithm.

-- Method 2, BFS (brute-force)