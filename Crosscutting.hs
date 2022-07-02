module Crosscutting where

type Name = String

data FunF = MkVecF | MkMatF | MaxF | MinF | AddF | SubF | MulF | DivF | ClampF | SinF | CosF | AtanF | SqrtF | LengthF | AbsF
  deriving Show

data TypeF = ScalarF | VectorF | MatrixF
  deriving (Eq, Show)
