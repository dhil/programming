{--- Option/Maybe monad implementation by hand ---}

-- | The data type
data Option a = None
              | Some a
              deriving Show

-- | Option is a functor
instance Functor Option where
  fmap f (Some x) = Some (f x)
  fmap _  _       = None

-- | Option is also an applicative functor
instance Applicative Option where
  pure x = Some x
  (Some f) <*> x = fmap f x
  None     <*> _ = None

-- | Option is a monad
instance Monad Option where
  return = pure
  (Some x) >>= k = k x
  None     >>= _ = None
  fail _  = None
