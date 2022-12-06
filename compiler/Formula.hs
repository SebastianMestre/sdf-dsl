{-# LANGUAGE FlexibleInstances #-}

module Formula
  ( Formula

  , varF
  , constantF

  , letF

  -- vector operations
  , projectionF
  , getX
  , getY
  , getZ
  , mkVecF
  , mkMatF

  -- function calls
  , lengthF
  , minF
  , maxF
  , modF
  , mixF
  , clampF

  ) where

import Crosscutting
import FormulaAst

type Formula = Form ()

instance Num Formula where
  x + y         = AppF () AddF [x, y]
  x * y         = AppF () MulF [x, y]
  x - y         = AppF () SubF [x, y]
  negate x      = 0 - x
  abs           = undefined
  fromInteger n = LitF () (fromInteger n :: Float)
  signum        = undefined

instance Fractional Formula where
  x / y          = AppF () DivF [x, y]
  fromRational n = LitF () (fromRational n :: Float)

appF = AppF ()
varF = VarF ()
constantF = LitF ()
letF = LetF ()

projectionF = PrjF ()
getX = projectionF XF
getY = projectionF YF
getZ = projectionF ZF
mkVecF x y z = appF MkVecF [x, y, z]
mkMatF x y z = appF MkMatF [x, y, z]

lengthF f = appF LengthF [f]
minF f1 f2 = appF MinF [f1, f2]
maxF f1 f2 = appF MaxF [f1, f2]
modF x y = appF ModF [x, y]
clampF f1 f2 f3 = appF ClampF [f1, f2, f3]
mixF f1 f2 f3 = appF MixF [f1, f2, f3]

