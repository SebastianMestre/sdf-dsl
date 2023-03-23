module Lower where

import FormulaAst
import Crosscutting
import qualified VarLookup
import Ssa
import Control.Monad.State
import Control.Monad

type S d = ([d], VarId)

type GEnv = VarLookup.Lookup VarId

data NameResolution d a = NR { runNameResolution :: (GEnv, S d) -> (a, S d) }

instance Functor (NameResolution d) where
  fmap f mx = mx >>= (return . f)

instance Applicative (NameResolution d) where
  pure = return
  (<*>) = ap

instance Monad (NameResolution d) where
  return a = NR $ \(_, s) -> (a, s)
  ma >>= g = NR $ \(env, s) -> let
    (a, s') = runNameResolution ma (env, s)
    (b, s'') = runNameResolution (g a) (env, s')
    in (b, s'')

getState :: NameResolution d (S d)
getState = NR $ \(_, s) -> (s, s)

putState :: S d -> NameResolution d ()
putState s = NR $ \_ -> ((), s)

getEnv :: NameResolution d GEnv
getEnv = NR $ \(env, s) -> (env, s)

extendEnv :: (String, VarId) -> NameResolution d a -> NameResolution d a
extendEnv p ma = NR $ \(env, s) -> runNameResolution ma (VarLookup.extend p env, s)

addSsa :: Ssa -> NameResolution Ssa VarId
addSsa x = do
  (xs, n) <- getState
  putState (xs ++ [x], n+1)
  return n

makeVar :: SsaArg -> NameResolution Ssa VarId
makeVar (SsaVar i)   = return i
makeVar (SsaConst x) = addSsa (ConstT x)

addSsa' :: Form TypeF -> NameResolution (Form TypeF) VarId
addSsa' x = do
  (xs, n) <- getState
  putState (xs ++ [x], n+1)
  return n

makeVar' :: Form TypeF -> NameResolution (Form TypeF) VarId
makeVar' x = addSsa' x

lower :: Form TypeF -> [Ssa]
lower f = ssa
  where

  (lastValue, (ssa, _)) = runNameResolution (go f) initial

  initial = (defaultEnv, defaultState)

  defaultEnv :: GEnv
  defaultEnv = VarLookup.extend ("pos", 0) $ VarLookup.empty

  defaultState :: S Ssa
  defaultState = ([VarT VectorF "pos"], 1)

  processVarF' _ v        = VarLookup.get v <$> getEnv
  processLitF' _ x        = pure x
  processAppF' t op as    = addSsa' =<< (AppF t op <$> mapM go' as)
  processPrjF' t field e1 = addSsa' =<< (PrjF t field <$> go' e1)

  go' :: Form TypeF -> NameResolution (Form TypeF) (Form TypeF)
  go' (VarF t v)         = VarF t <$> show <$> processVarF' t v
  go' (LitF t x)         = LitF t <$> processLitF' t x
  go' (AppF t op as)     = VarF t <$> show <$> processAppF' t op as
  go' (PrjF t field e1)  = VarF t <$> show <$> processPrjF' t field e1
  go' (LetF t h x f1 f2) = do
    a1 <- go' f1
    i1 <- makeVar' a1
    i2 <- extendEnv (x, i1) $ go' f2
    return i2

  processVarF _ v        = VarLookup.get v <$> getEnv
  processLitF _ x        = pure x
  processAppF t op as    = addSsa =<< (AppT t op <$> mapM go as)
  processPrjF _ field e1 = addSsa =<< (PrjT field <$> go e1)

  go :: Form TypeF -> NameResolution Ssa SsaArg
  go (VarF t v)         = SsaVar   <$> processVarF t v
  go (LitF t x)         = SsaConst <$> processLitF t x
  go (AppF t op as)     = SsaVar   <$> processAppF t op as
  go (PrjF t field e1)  = SsaVar   <$> processPrjF t field e1
  go (LetF t h x f1 f2) = do
    a1 <- go f1
    i1 <- makeVar a1
    i2 <- extendEnv (x, i1) $ go f2
    return i2
