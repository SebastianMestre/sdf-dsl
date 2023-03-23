module FormulaAst where

{--

Este modulo implementa el AST de un lenguaje para expresar
formulas matemáticas.

Cada let está anotado con el tipo de la variable que liga.

--}

import Crosscutting

data Form
  = LetF TypeF Name Form Form
  | VarF Name
  | AppF FunF [Form]
  | LitF Float
  | PrjF FieldF Form
  deriving Show

