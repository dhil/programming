{-#LANGUAGE GADTs #-}
{-- Normalisation by Evaluation for simply typed lambda calculus --}
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

freshName :: () -> Int
freshName _ =
  unsafePerformIO $ do
    x <- readIORef counter
    writeIORef counter (x + 1)
    return x


-- Type structure
data Ty where
  TyUnit :: Ty
  TyArrow :: Ty -> Ty -> Ty
  TyProd :: Ty -> Ty -> Ty

instance Show Ty where
  show  TyUnit         = "()"
  show (TyArrow t1 t2) =
    case t1 of
      TyArrow _ _ -> "(" ++ show t1 ++ ")"
      _           -> show t1
    ++ " -> " ++ show t2
  show (TyProd t1 t2) =
    case (t1,t2) of
      ((TyArrow _ _), (TyArrow _ _)) -> "(" ++ show t1 ++ ") * (" ++ show t2 ++ ")"
      ((TyArrow _ _), _) -> "(" ++ show t1 ++ ") * " ++ show t2
      (_, (TyArrow _ _)) -> show t1 ++ " * (" ++ show t2 ++ ")"
      (_,_)              -> show t1 ++ " * " ++ show t2

-- Term structure
data Tm where
  TmVar  :: String -> Tm
  TmLam  :: String -> Tm -> Tm
  TmApp  :: Tm -> Tm -> Tm
  TmPair :: Tm -> Tm -> Tm
  TmFst  :: Tm -> Tm
  TmSnd  :: Tm -> Tm

instance Show Tm where
  show (TmVar name) = name
  show (TmLam binder body) = "λ" ++ binder ++ ". " ++ show body
  show (TmFst t) = "fst " ++
                   case t of
                     TmPair _ _ -> show t
                     TmVar _ -> show t
                     _ -> "(" ++ show t ++ ")"
  show (TmSnd t) = "snd " ++
                   case t of
                     TmPair _ _ -> show t
                     TmVar _ -> show t
                     _ -> "(" ++ show t ++ ")"
  show (TmPair t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (TmApp t1 t2) =
    case t1 of
      TmLam _ _ -> "(" ++ show t1 ++ ")"
      TmFst _   -> "(" ++ show t1 ++ ")"
      TmSnd _   -> "(" ++ show t1 ++ ")"
      _         -> show t1
    ++ " " ++
    case t2 of
      TmLam _ _ -> "(" ++ show t2 ++ ")"
      TmFst _   -> "(" ++ show t2 ++ ")"
      TmSnd _   -> "(" ++ show t2 ++ ")"
      _         -> show t2

-- Semantics
data Sem where
  SemLam  :: (Sem -> Sem) -> Sem
  SemPair :: Sem -> Sem -> Sem
  SemSyn  :: Tm -> Sem


reflect :: Ty -> Tm -> Sem
reflect (TyArrow a b) tm =
  SemLam (\s -> reflect b (TmApp tm (reify a s)))
reflect (TyProd a b) tm =
  SemPair (reflect a (TmFst tm)) (reflect b (TmSnd tm))
reflect TyUnit tm = SemSyn tm

reify :: Ty -> Sem -> Tm
reify (TyArrow a b) (SemLam s) =
  let x = show $ freshName () in
  TmLam x (reify b (s (reflect a (TmVar x))))
reify (TyProd a b) (SemPair x y) =
  TmPair (reify a x) (reify b y)
reify TyUnit (SemSyn t) = t

type Env = [(String, Sem)]

meaning :: Env -> Tm -> Sem
meaning ctx tm =
  case tm of
    TmVar x ->
      case lookup x ctx of
        Just s -> s
        Nothing -> error "N/A"
    TmLam x b -> SemLam (\s -> meaning ((x,s) : ctx) b)
    TmApp a b ->
      case meaning ctx a of
        SemLam s -> s (meaning ctx b)
    TmPair a b -> SemPair (meaning ctx a) (meaning ctx b)
    TmFst a    ->
      case meaning ctx a of
        SemPair a _ -> a
    TmSnd a    ->
      case meaning ctx a of
        SemPair _ b -> b

nbe :: Ty -> Tm -> Tm
nbe ty tm = reify ty (meaning [] tm)

-- () -> () -> ()
k :: Tm
k = (TmLam "x" (TmLam "y" (TmVar "x")))

-- (() -> () -> ()) -> (() -> ()) -> () -> ()
s :: Tm
--     lam("x",   lam("y",   lam("z",   app (  app (  var "x",    var "z"),    app (  var "y",    var "z")))))
s = TmLam "x" (TmLam "y" (TmLam "z" (TmApp (TmApp (TmVar "x") (TmVar "z")) (TmApp (TmVar "y") (TmVar "z")))))

skk :: Tm
skk = TmApp (TmApp s k) k


-- Examples
ex1 :: Tm
ex1 = nbe (TyArrow TyUnit TyUnit) skk

ex2 :: Tm
ex2 = nbe (TyArrow (TyArrow TyUnit TyUnit) (TyArrow TyUnit TyUnit)) skk
