module Lower where

import FormulaAst
import Crosscutting
import qualified VarLookup
import Ssa
import Control.Monad.State
import Control.Monad

type S = ([Ssa], VarId)

type GEnv = VarLookup.Lookup VarId

data NameResolution a = NR { runNameResolution :: (GEnv, S) -> (a, S) }

instance Functor NameResolution where
  fmap f mx = mx >>= (return . f)

instance Applicative NameResolution where
  pure = return
  (<*>) = ap

instance Monad NameResolution where
  return a = NR $ \(_, s) -> (a, s)
  ma >>= g = NR $ \(env, s) -> let
    (a, s') = runNameResolution ma (env, s)
    (b, s'') = runNameResolution (g a) (env, s')
    in (b, s'')

getState :: NameResolution S
getState = NR $ \(_, s) -> (s, s)

putState :: S -> NameResolution ()
putState s = NR $ \_ -> ((), s)

getEnv :: NameResolution GEnv
getEnv = NR $ \(env, s) -> (env, s)

extendEnv :: (String, VarId) -> NameResolution a -> NameResolution a
extendEnv p ma = NR $ \(env, s) -> runNameResolution ma (VarLookup.extend p env, s)

addSsa :: Ssa -> NameResolution VarId
addSsa x = do
  (xs, n) <- getState
  putState (xs ++ [x], n+1)
  return n

stringifyVar i = VarT undefined ("v" ++ show i)

lower :: Form TypeF -> (Ssa, [Ssa])
lower f = (lastValue, ssa)
  where

  (lastValue, (ssa, _)) = runNameResolution (go' f) initial

  initial = (defaultEnv, defaultState)

  defaultEnv :: GEnv
  defaultEnv = VarLookup.extend ("pos", 0) $ VarLookup.empty

  defaultState :: S
  defaultState = ([VarT VectorF "pos"], 1)

  go' :: Form TypeF -> NameResolution Ssa
  go' (VarF t v)         = stringifyVar <$> VarLookup.get v <$> getEnv
  go' (LitF ty x)        = ConstT <$> pure x
  go' (AppF t op as)     = AppT t op <$> mapM go' as
  go' (PrjF _ field e1)  = PrjT field <$> go' e1
  go' (LetF t h x f1 f2) = do
    i1 <- addSsa =<< go' f1
    i2 <- extendEnv (x, i1) $ go' f2
    return i2
