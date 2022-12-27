module Lower where

import FormulaAst
import Crosscutting
import qualified VarLookup
import Ir
import Control.Monad.State

type S = ([Ssa], VarId)

type GEnv = VarLookup.Lookup VarId

addSsa :: Ssa -> State S VarId
addSsa x = do
  (xs, n) <- get
  put (xs ++ [x], n+1)
  return n

makeVar :: SsaArg -> State S VarId
makeVar (TaVar i) = return i
makeVar (TaConst x) = addSsa (ConstT x)

lower :: Form TypeF -> [Ssa]
lower f = fst $ snd $ runState (go defaultEnv f) defaultState
  where

  defaultEnv :: GEnv
  defaultEnv = VarLookup.extend ("pos", 0) $ VarLookup.empty

  defaultState :: S
  defaultState = ([VarT VectorF "pos"], 1)

  go :: GEnv -> Form TypeF -> State S SsaArg
  go env (LetF t h x f1 f2) = do
    a1 <- go env f1
    i1 <- makeVar a1
    i2 <- go (VarLookup.extend (x, i1) env) f2
    return i2
  go env (VarF t v) = do
    let i1 = VarLookup.get v env
    return $ TaVar i1
  go env (LitF ty x) = do
    return $ TaConst x
  go env (AppF t op as) = do
    is <- mapM (go env) as
    i <- addSsa (AppT t op is)
    return $ TaVar i
  go env (PrjF _ field e1) = do
    i1 <- go env e1
    i <- addSsa (PrjT field i1)
    return $ TaVar i
