-- Making Change
-- You are given n types of coin denominations of values v(1) < v(2) <
-- ... < v(n) (all integers).  Assume v(1) = 1, so you can always make
-- change for any amount of money C. Give an algorithm which makes
-- change for an amount of money C with as few coins as possible.

-- Let M(j) be the minimum number of coins required to make change for
-- amount of money j.  Equation: M(j) = min_i {M(j - v_i)} + 1

denominations :: [Integer]
denominations = [1,5,10,20,50,100]

change :: Integer -> Integer
change n
  | n <= 0 = 0
  | n `elem` denominations = n
change j = minimum $ filter (>0) $ map (\v -> 1 + change (j - v)) denominations
