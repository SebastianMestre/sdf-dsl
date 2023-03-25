{-

Este modulo convierte de Form a FormNl. En particular,
extrae todos los lets y los pone en una lista aparte.

-}
module HoistLets where

import Core
import Crosscutting
import qualified VarLookup
import Control.Monad

{-

Definimos una monada especifica para este paso. Implementa
los efectos Reader y State.

El efecto Reader se usa para pasar el entorno y extenderlo a
medida que nos adentramos en let-bindings.

El efecto State se usa para llevar las variables ya ligadas
y un contador que usamos para generar variables frescas.

-}

type State = ([DeclN], VarId)

type Env = VarLookup.Lookup VarId

data NameResolution a = NR { runNameResolution :: (Env, State) -> (a, State) }

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

getState :: NameResolution State
getState = NR $ \(_, s) -> (s, s)

putState :: State -> NameResolution ()
putState s = NR $ \_ -> ((), s)

getEnv :: NameResolution Env
getEnv = NR $ \(env, s) -> (env, s)

extendEnv :: (String, VarId) -> NameResolution a -> NameResolution a
extendEnv p ma = NR $ \(env, s) -> runNameResolution ma (VarLookup.extend p env, s)

addDecl :: DeclN -> NameResolution VarId
addDecl x = do
  (xs, n) <- getState
  putState (xs ++ [x], n+1)
  return n

hoistLets :: Form -> (FormNl, [DeclN])
hoistLets f = (lastValue, decls)
  where

  (lastValue, (decls, _)) = runNameResolution (go f) initial

  initial = (defaultEnv, defaultState)

  defaultEnv :: Env
  defaultEnv = VarLookup.extend ("pos", 0) $ VarLookup.empty

  defaultState :: State
  defaultState = ([DeclN VectorF $ FreeN "pos"], 1)

  go :: Form -> NameResolution FormNl
  go (VarF v)         = BoundN <$> VarLookup.get v <$> getEnv
  go (LitF x)         = LitN <$> pure x
  go (AppF op as)     = AppN op <$> mapM go as
  go (PrjF field e1)  = PrjN field <$> go e1
  go (LetF h x f1 f2) = do
    i1 <- addDecl =<< DeclN h <$> go f1
    i2 <- extendEnv (x, i1) $ go f2
    return i2
