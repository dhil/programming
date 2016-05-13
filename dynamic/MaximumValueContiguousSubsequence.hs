-- Maximum Value Contiguous Subsequence
-- Given a sequence of n real numbers A(1) ... A(n), determine a
-- contiguous subsequence A(i) ... A(j) for which the sum of elements
-- in the subsequence is maximized.

-- Equations: Let M(j) denote the maximum sum over all windows ending at j.
-- M(j) = max{M(j-1) + A[j], A[j]}. Intuition: Either we have to
-- extend an optional window (M(j-1) + A[j]) or start a new one (A[j]).

mySequence :: (Num a, Ord a) => [a]
mySequence = [1,3,4,5,8,-2,3,-5,-7]

maxCSeq :: (Num a, Ord a) => [a] -> a
maxCSeq xs
  = snd $ foldl (\(mj,max') x ->
                  let n = max (mj + x) x
                  in (n,if n > max'
                        then n
                        else max')
          ) (0,0) xs
