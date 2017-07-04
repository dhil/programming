{--- State monad implementation by hand ---}

-- | The data type
newtype State s a = State { runState :: (s -> (a, s)) }

-- | Algebraic operations
get :: State s s
get = State (\st -> (st,st))

put :: s -> State s ()
put x = State (\st -> ((),x))

-- | State is a functor
instance Functor (State s) where
  fmap f m = State (\st -> let (x, st') = runState m st in
                           (f x, st'))

-- | State is an applicative functor
instance Applicative (State s) where
  pure x    = State (\st -> (x, st))
  m1 <*> m2 = State (\st -> let (f, st') = runState m1 st in
                            runState (fmap f m2) st')

-- | State is a monad
instance Monad (State s) where
  return = pure
  m >>= k = State (\st -> let (x, st') = runState m st in
                          runState (k x) st')

-- | Other runners
evalState :: State s a -> s -> a
evalState m = fst . (runState m)

execState :: State s a -> s -> s
execState m = snd . (runState m)

-- | Examples
incr :: State Int ()
incr = do st <- get
          put (st+1)

decr :: State Int ()
decr = do st <- get
          put (st-1)

toZero :: Int -> Int
toZero n = evalState toZero' n
  where
    toZero' :: State Int Int
    toZero' = do n <- get
                 if n < 0 then
                   do incr
                      toZero'
                 else if n > 0 then
                   do decr
                      toZero'
                 else
                   return n

collatz :: Int -> (Int, Int)
collatz n = runState (countCalls n) 0
  where
    f :: Int -> Int
    f n
     | n `mod` 2 == 0 = n `div` 2
     | otherwise = 3 * n + 1
    countCalls :: Int -> State Int Int
    countCalls n =
      if n == 1 then
        return n
      else
        do incr
           countCalls (f n)
