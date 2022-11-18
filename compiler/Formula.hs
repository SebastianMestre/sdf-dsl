{-# LANGUAGE FlexibleInstances #-}

module Formula where

import Crosscutting

data Form a
  = LetF a TypeF Name (Form a) (Form a)
  | VarF a Name
  | AppF a FunF [(Form a)]
  | LitF Float
  | PrjF a FieldF (Form a)
  deriving Show

instance Num (Form ()) where
  x + y         = AppF () AddF [x, y]
  x * y         = AppF () MulF [x, y]
  x - y         = AppF () SubF [x, y]
  negate x      = 0 - x
  abs           = undefined
  fromInteger n = LitF (fromInteger n :: Float)
  signum        = undefined

instance Fractional (Form ()) where
  x / y          = AppF () DivF [x, y]
  fromRational n = LitF (fromRational n :: Float)
