{-

Este modulo ofrece herramientas para construir valores de tipo Form

-}
module Form
  ( Form

  -- posicion en el mundo
  , pos

  -- constante escalar
  , constantF

  -- declaracion de variable
  , withLocal

  -- operaciones de vectores
  , getX
  , getY
  , getZ
  , mkVecF
  , mkMatF

  -- llamadas a funcion
  , lengthF
  , modF
  , minF
  , maxF
  , clampF
  , mixF

  ) where

import Crosscutting
import Core

-- Implementamos Num y Fractional para permitir el uso de
-- azucar sintactico al crear terminos

instance Num Form where
  x + y         = AppF AddF [x, y]
  x * y         = AppF MulF [x, y]
  x - y         = AppF SubF [x, y]
  negate x      = 0 - x
  abs x         = AppF AbsF [x]
  fromInteger n = LitF (fromInteger n :: Float)
  signum        = undefined

instance Fractional Form where
  x / y          = AppF DivF [x, y]
  fromRational n = LitF (fromRational n :: Float)

pos :: Form
pos = VarF "pos"

constantF :: Float -> Form
constantF = LitF

withLocal :: String -> TypeF -> Form -> (Form -> Form) -> Form
withLocal v ty t f = LetF ty v t (f (VarF v))

getX = PrjF XF
getY = PrjF YF
getZ = PrjF ZF
mkVecF x y z = AppF MkVecF [x, y, z]
mkMatF x y z = AppF MkMatF [x, y, z]

lengthF t    = AppF LengthF [t]
modF t u     = AppF ModF [t, u]
minF t u     = AppF MinF [t, u]
maxF t u     = AppF MaxF [t, u]
clampF t u s = AppF ClampF [t, u, s]
mixF t u s   = AppF MixF [t, u, s]
