-- Making Change
-- You are given n types of coin denominations of values v(1) < v(2) <
-- ... < v(n) (all integers).  Assume v(1) = 1, so you can always make
-- change for any amount of money C. Give an algorithm which makes
-- change for an amount of money C with as few coins as possible.

-- Let M(j) be the minimum number of coins required to make change for
-- amount of money j.  Equation: M(j) = min_i {M(j - v_i)} + 1

-- Solution adapted from https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/coinChange

import Data.Array

type Coin = Integer

denominations :: [Coin]
denominations = [1,5,10,20,50,100]

change :: [Coin] -> Integer -> Maybe (Integer, [Coin])
change coins amount = get numCoins amount
     where
       m = array ((0,0), (numCoins, amount)) [((i,c), takeCoin i c) | i <- [0..numCoins], c <- [0..amount]]
       get i c
         | c < 0 || i < 0 = Nothing
         | c == 0         = Just (0, [])
         | otherwise      = m ! (i, c)
       numCoins = length coins - 1
       takeCoin i c
         | coin > c   = get (i-1) c
         | otherwise  = case (get i (c - coin), get (i - 1) c) of
                          (Just (n, t), Just (n', t')) -> Just $ if n+1 <= n' then (n+1, coin : t) else (n', t')
                          (Nothing,     Just (n', t')) -> Just (n', t')
                          (Just (n, t), Nothing)       -> Just (n+1, coin : t)
                          (Nothing,     Nothing)       -> Nothing
         where coin = coins !! i
                                               
