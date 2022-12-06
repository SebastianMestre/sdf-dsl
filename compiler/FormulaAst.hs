module FormulaAst where

import Crosscutting

data Form a
  = LetF a TypeF Name (Form a) (Form a)
  | VarF a Name
  | AppF a FunF [(Form a)]
  | LitF a Float
  | PrjF a FieldF (Form a)
  deriving Show

