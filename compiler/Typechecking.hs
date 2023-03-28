{-

Este modulo implementa un typechecker para el lenguage
intermedio. En el lenguaje que se le presenta al usuario
no es posible escribir terminos mal tipados. La principal
utilidad es detectar errores en el propio compilador.

Para permitir una expresion natural y similar a GLSL,
tenemos sobrecarga de tipos en las funciones del lenguaje.

Representamos una sobrecarga de funcion con una lista de
tipos. El ultimo representa el tipo de retorno. El resto son
los argumentos.

-}
module Typechecking where

import Core
import Crosscutting
import Data.Maybe


type Err = Either String
type TEnv = [(Name, TypeF)]
type Overload = [TypeF]

returnType :: Overload -> TypeF
returnType = last

paramTypes :: Overload -> [TypeF]
paramTypes = init

overloadMatches :: Overload -> [TypeF] -> Bool
overloadMatches argTypes overload = argTypes == paramTypes overload

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

infer :: TEnv -> Form -> Err TypeF
infer env (LetF t x e1 e2) = do
  t1' <- infer env e1
  if t1' /= t
    then error "mismatched types"
    else return ()
  t2' <- infer ((x, t) : env) e2
  return t2'

infer env (VarF x) = do
  let mt' = lookup x env
  t' <- if isNothing mt'
    then error ("undefined variable: " ++ x)
    else return $ fromJust mt'
  return t'

infer env (AppF f as) = do
  argTypes <- mapM (infer env) as

  let overloads = overloadsOf f 
  let viable = filter (overloadMatches argTypes) overloads

  chosen <- case viable of
    [x] -> return x
    []  -> error ("no overloads for " ++ show f ++ " argument types are: " ++ show argTypes ++ " args are: " ++ show as)
    _   -> error "ambiguous overloads"

  let t = returnType chosen
  return t

infer env (LitF x) = do
  return ScalarF

infer env (PrjF field e) = do
  t <- infer env e

  inner <- if t /= VectorF
    then error "accessed fields of something that is not a vector"
    else return ()

  return ScalarF
