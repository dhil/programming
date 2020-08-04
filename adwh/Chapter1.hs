--- Solutions to exercises in Chapter 1.


{--

Fusion law example, page 12:

Work out the right hand side of the following incomplete fusion law

   foldr f e . concat = ??

My solution:

I'll derive a solution to the above equation from first principles
without using the "master fusion rule".

The (implicit) goal of this exercise is to fuse the separate list
traversals by `foldr` and `concat` into one.

First let's recall the type signatures of `foldr` and `concat`

  foldr : (a -> b -> b) -> b -> [a] -> b
  concat : [[c]] -> [c]

Thus their composition has type

  foldcat : (c -> b -> b) -> b -> [[c]] -> b
  foldcat f e = foldr f e . concat

Operationally, `foldr` applies its function-argument point-wise to its
list-argument; a possible implementation is

  foldr f e []       = e
  foldr f e (x : xs) = f x (foldr f e xs)

Operationally, `concat` folds its argument point-wise; a possible
implementation of `concat` is

  concat = foldr (++) []

where `++` itself may be defined in terms of `foldr`

  ++ : [a] -> [a] -> [a]
  xs ++ ys = foldr (:) ys xs

with this in mind, we can "derive" the right hand side

  foldr f e . concat
= { definition of `concat` }
  foldr f e . (foldr (++) [])
= { definition of `++` }
  foldr f e . (foldr (foldr (:)) [])
= { hand-wavy: the left-most and right-most `foldr` operate on the
    same data, i.e. [a], with the middle `foldr` driving the
    iteration.  Thus we can eliminate either of the left- or
    right-most `foldr` expression by combining their
    function-arguments, doing so requires us to generalise the initial
    value `[]` in the middle `foldr` expression to be `e`. }
  foldr (\xs e' -> foldr f e' xs) e
= { definition of `flip` }
  foldr (flip (foldr f)) e

Of course, using the master fusion rule is much cleaner and more
rigorous.

For the "simpler" case
  foldr f e (xs ++ ys)
= { by similar reasoning to the above }
  foldr f (foldr f e ys) xs

--}
