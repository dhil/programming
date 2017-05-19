{--- List monad implementation by hand ---}
import Prelude hiding (map,concat)

-- | The data type
data List a = Nil
            | Cons a (List a)

-- | Show instance for pretty printing
instance Show a => Show (List a) where
  show Nil = "[]"
  show xs  = "[" ++ display xs ++ "]"
        where
          display (Cons x Nil) = show x
          display (Cons x xs)  = show x ++ "," ++ (display xs)

-- | Some standard functions on lists
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append xs Nil = xs
append (Cons x xs) ys = Cons x (append xs ys)

concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons xs xss) = append xs (concat xss)

-- | List is a functor
instance Functor List where
  fmap f xs = map f xs

-- | List is an applicative functor
instance Applicative List where
  pure x = Cons x Nil
  Nil         <*> _  = Nil
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

-- | List is a monad
instance Monad List where
  return = pure
  xs >>= k = concat (fmap k xs)
  fail _ = Nil
