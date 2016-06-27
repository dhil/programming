module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st name val = \name' -> if name == name' then val else st name'

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st expr = case expr of
                Var name      -> st name
                Val i         -> i
                Op lhs op rhs -> evalE st lhs `binop` evalE st rhs
                   where
                     binop = \l r -> case op of
                                     Plus   -> l + r
                                     Minus  -> l - r
                                     Times  -> l * r
                                     Divide -> l `div` r
                                     _      -> fromEnum $ case op of
                                                          Gt     -> l > r
                                                          Ge     -> l >= r
                                                          Lt     -> l < r
                                                          Le     -> l <= r
                                                          Eql    -> l == r
                                                          _ -> undefined -- Gets rid of false positive warning about non-exhaustive patterns
                                                          

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign name expr)       = DAssign name expr
desugar (Incr name)              = DAssign name (Op (Var name) Plus (Val 1))
desugar (If expr btrue bfalse)   = DIf expr (desugar btrue) (desugar bfalse)
desugar (While expr body)        = DWhile expr (desugar body)
desugar (For pre expr post body) = DSequence (desugar pre)
                                             (DWhile expr (DSequence (desugar body) (desugar post)))
desugar (Sequence stmt1 stmt2)   = DSequence (desugar stmt1) (desugar stmt2)
desugar Skip                     = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st stmt = case stmt of
                     DSkip                 -> st
                     DAssign name expr     -> extend st name (evalE st expr)
                                              --
                     DIf expr btrue bfalse -> if evalE st expr == 1
                                              then evalSimple st btrue
                                              else evalSimple st bfalse
                                              --
                     DWhile expr body      -> if evalE st expr == 1
                                              then let st' = evalSimple st body
                                                   in  evalSimple st' stmt
                                              else st
                                              --
                     DSequence stmt1 stmt2 -> evalSimple (evalSimple st stmt1) stmt2

run :: State -> Statement -> State
run st = evalSimple st . desugar

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "A" (Var "In")
                   , Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   , Assign "Out" (Var "B")
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]

count10 :: Statement
count10 = slist [ Assign "B" (Val 10)
                  , For (Assign "A" (Var "In"))
                        (Op (Var "A") Lt (Val 10))
                        (Incr "A")
                        (Assign "B" (Op (Var "B") Minus (Val 1)))
                  , Assign "Out" (Var "B")
                  ]
