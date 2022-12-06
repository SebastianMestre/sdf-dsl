module Typechecking where

import FormulaAst
import Crosscutting
import Data.Maybe

type TEnv = [(Name, TypeF)]

-- First is return type, then come all the parameter types
type Overload = [TypeF]

overloadMatches :: Overload -> [TypeF] -> Bool
overloadMatches argTypes overload = argTypes == paramTypes overload

returnType :: Overload -> TypeF
returnType = last

paramTypes :: Overload -> [TypeF]
paramTypes = init

overloadsOf :: FunF -> [Overload]
overloadsOf MkVecF = [[ScalarF, ScalarF, ScalarF, VectorF]]
overloadsOf MkMatF = [[VectorF, VectorF, VectorF, MatrixF]]
overloadsOf MaxF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf MinF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf AddF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf SubF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf MulF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF], [MatrixF, VectorF, VectorF]]
overloadsOf DivF = [[ScalarF, ScalarF, ScalarF]]
overloadsOf ModF = [[ScalarF, ScalarF, ScalarF]]
overloadsOf ClampF = [[ScalarF, ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF, VectorF]]
overloadsOf SinF = [[ScalarF, ScalarF]]
overloadsOf CosF = [[ScalarF, ScalarF]]
overloadsOf AtanF = [[ScalarF, ScalarF, ScalarF]]
overloadsOf SqrtF = [[ScalarF, ScalarF]]
overloadsOf LengthF = [[VectorF, ScalarF]]
overloadsOf AbsF = [[ScalarF, ScalarF], [VectorF, VectorF], [MatrixF, MatrixF]]
overloadsOf MixF = [[ScalarF, ScalarF, ScalarF, ScalarF]]

type Err = Either String

infer :: TEnv -> Form () -> Err (TypeF, Form TypeF)
infer env (LetF () t x e1 e2) = do
  (t1', e1') <- infer env e1
  if t1' /= t
    then fail "mismatched types"
    else return ()
  (t2', e2') <- infer ((x, t) : env) e2 
  return (t2', LetF t2' t x e1' e2')

infer env (VarF () x) = do
  let mt' = lookup x env
  t' <- if isNothing mt'
    then fail ("undefined variable: " ++ x)
    else return $ fromJust mt'
  return (t', VarF t' x)

infer env (AppF () f as) = do
  as' <- mapM (infer env) as

  let args = map snd as'
  let argTypes = map fst as'

  let overloads = overloadsOf f 
  let viable = filter (overloadMatches argTypes) overloads

  chosen <- case viable of
    [x] -> return x
    []  -> fail ("no overloads for " ++ show f ++ " argument types are: " ++ show argTypes ++ " args are: " ++ show args)
    _   -> fail "ambiguous overloads"

  let t = returnType chosen
  return (t, AppF t f args)

infer env (LitF () x) = do
  return (ScalarF, LitF ScalarF x)

infer env (PrjF () field e) = do
  (t, e') <- infer env e

  inner <- if t /= VectorF
    then fail "accessed fields of something that is not a vector" -- TODO: error message
    else return ()

  return $ (ScalarF, PrjF ScalarF field e')
