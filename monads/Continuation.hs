{--- Continuation monad implementation by hand ---}
import qualified System.IO.Unsafe as U
import qualified Data.Char as C

-- | The data type
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

cont :: ((a -> r) -> r) -> Cont r a
cont k = Cont k

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = cont (\k -> runCont
                  (f (\x -> cont (\_ -> k x))) k)

-- | Cont is a functor
instance Functor (Cont r) where
  fmap f k = cont (\g -> runCont k (\x -> g (f x)))

-- | Cont is an applicative functor
instance Applicative (Cont r) where
  pure x  = cont (\k -> k x)
  k <*> k' = cont (\r -> runCont k
                   (\k'' -> runCont k'
                     (\x -> r (k'' x))))

-- | Cont is a monad
instance Monad (Cont r) where
  return  = pure
  m >>= k = cont (\k' -> runCont m
                   (\x -> runCont (k x)
                     (\y -> k' y)))

-- | Examples
printDoubleLen :: Foldable t => t a -> IO ()
printDoubleLen t = runCont
                   ((pure $ length t) >>= (\x -> pure $ x + x))
                   print


hello :: Cont String Int
hello = do
  x <- return 1
  y <- sayHello
  z <- return 2
  return $ x + y + z
  where
    sayHello :: Cont String Int
    sayHello = cont (\_ -> "Hello!")


raise :: e -> (e -> Cont r a) -> Cont r a
raise exn handler = handler exn

div' :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
div' x y k =
  callCC $ \k' ->
             do r <- callCC $ \k'' ->
                  if y == 0 then
                    raise "division by zero" k''
                  else
                    k' $ x `div` y
                k r

safeDiv :: Int -> Int -> Int
safeDiv x y = runCont (div' x y (\_ -> pure 0)) id
