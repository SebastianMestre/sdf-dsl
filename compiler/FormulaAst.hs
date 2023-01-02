module FormulaAst where

{--

Este modulo implementa el AST de un lenguaje simplemente
tipado para expresar formulas matemáticas.

Cada let está anotado con el tipo de la variable que liga.

Aparte, cada nodo está anotado con un valor del tipo
paramétrico ``t``. El propósito de esto es poder decorar
cada nodo con su tipo, de manera opcional.

En particular, el tipo ``Form ()`` representa términos sin
tipar y el tipo ``Form TypeF`` representa términos tipados.

La función ``Typechecking.infer`` implementa esa conversion.

--}

import Crosscutting

data Form t
  = LetF t TypeF Name (Form t) (Form t)
  | VarF t Name
  | AppF t FunF [(Form t)]
  | LitF t Float
  | PrjF t FieldF (Form t)
  deriving Show

