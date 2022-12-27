module Ssa where

import Crosscutting

type VarId = Int

data SsaArg = TaVar VarId | TaConst Float
  deriving Show

data Ssa
  = AppT TypeF FunF [SsaArg]
  | VarT TypeF Name
  | ConstT Float
  | PrjT FieldF SsaArg
  deriving Show
