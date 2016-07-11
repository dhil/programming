{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P cs) == (P cs')
      | length cs /= length cs' = False
      | otherwise = cs == cs'
 
-- Exercise 3 -----------------------------------------

newtype Exponent = E Int

instance Show Exponent where
  show (E e)
    | e == 0 = ""
    | e == 1 = ""
    | otherwise = show e

instance (Num a, Eq a, Show a) => Show (Poly a) where  
    show (P cs)
      | all (==0) cs = "0"
      | otherwise = concat . intersperse " + " . reverse . filter (/="0") . map showTerm $ terms
      where
        terms = zip cs [0..]
        showTerm (c, e)
              | c == 0    = "0"                          -- 0
              | e == 0    = (show c)                     -- c  
              | c == 1 && e == 1    = "x"                -- x  
              | c == -1 && e == 1   = "-x"               -- -x
              | c == -1   = "-x^" ++ (show e)            -- -x^e
              | c == 1    = "x^" ++ (show e)             -- x^e        
              | e == 1    = (show c) ++ "x"              -- c*x
              | otherwise = (show c) ++ "x^" ++ (show e) -- c*x^e
                                                   

-- Exercise 4 -----------------------------------------

rpad :: Int -> b -> [b] -> [b]
rpad n u xs = xs ++ replicate n u

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P cs) (P cs') = P $ zipWith (+) (rpad n 0 cs) (rpad n' 0 cs')
                      where
                        m  = max (length cs) (length cs')
                        n  = abs m - (length cs)
                        n' = abs m - (length cs')

-- Exercise 5 -----------------------------------------

instance Functor Poly where
  fmap f (P cs) = P $ fmap f cs

lpad :: Int -> b -> [b] -> [b]
lpad n u xs = replicate n u ++ xs

lshift :: Num a => Int -> Poly a -> Poly a
lshift n (P cs) = P $ lpad n 0 cs

times :: Num a => Poly a -> Poly a -> Poly a
times (P cs) q = foldr (+) (P [0]) $
                 zipWith (\c n -> lshift n (fmap (*c) q)) cs [0..]

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P cs) = P $ map negate cs
    fromInteger n = P [fromIntegral n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P cs) x = sum $ zipWith (\c e -> c * x^e) cs [0..]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f = iterate deriv f !! n

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P cs) = P $ zipWith (*) (drop 1 cs) (iterate (+1) 1)

