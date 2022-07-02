module Ir where

import Crosscutting

type VarId = Int

data TacArg = TaVar VarId | TaConst Float
  deriving Show

data Tac
  = AppT TypeF FunF [TacArg]
  | VarT TypeF Name
  | ConstT Float
  deriving Show
