module Lower where

import FormulaAst
import Crosscutting
import qualified VarLookup
import Ssa
import Control.Monad.State
import Control.Monad

type S = ([Ssa], VarId)

type GEnv = VarLookup.Lookup VarId

data NameResolution a = NR { runNameResolution :: (Genv, S) -> (a, S) }

instance Functor NameResolution where
  fmap f mx = mx >>= (return . f)

instance Applicative NameResolution where
  pure = return
  (<*>) = ap

instance Monad NameResolution where
  return a = NR $ \(_, s) -> (a, s)
  ma >>= g = NR $ \(env, s) -> let
    (a, s') = runNameResolution f (env, s)
    (b, s'') = runNameResolution (g a) (env, s')
    in (b, s'')

getState :: NameResolution S
getState = NR $ \(_, s) -> (s, s)

getEnv :: NameResolution GEnv
getSnv = NR $ \(env, s) -> (env, s)

addSsa :: Ssa -> NameResolution VarId
addSsa x = do
  (xs, n) <- getState
  put (xs ++ [x], n+1)
  return n

makeVar :: SsaArg -> State S VarId
makeVar (SsaVar i) = return i
makeVar (SsaConst x) = addSsa (ConstT x)

lower :: Form TypeF -> [Ssa]
lower f = ssa
  where

  (lastValue, (ssa, _)) = runNameResolution (go f) defaultState 

  initial = (defaultEnv, defaultState)

  defaultEnv :: GEnv
  defaultEnv = VarLookup.extend ("pos", 0) $ VarLookup.empty

  defaultState :: S
  defaultState = ([VarT VectorF "pos"], 1)

  go :: Form TypeF -> NameResolution SsaArg
  go (VarF t v)         = SsaVar <$> VarLookup.get v <$> getEnv
  go (LitF ty x)        = SsaConst <$> pure x
  go (AppF t op as)     = SsaVar <$> addSsa (AppT t op <$> mapM go as)
  go (PrjF _ field e1)  = SsaVar <$> addSsa (PrjT field <$> go e1)
  go (LetF t h x f1 f2) = do
    a1 <- go f1
    i1 <- makeVar a1
    i2 <- extendEnv (x, i1) $ go f2
    return i2
