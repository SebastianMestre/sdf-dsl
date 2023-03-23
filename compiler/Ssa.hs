module Ssa where

{--

Este módulo define un lenguaje static single-assignment
(SSA), que se usa como representación intermedia en la
compilación a GLSL.

Cada sentencia representa la asignación a una variable
nueva, y está anotada con el tipo de la variable (excepto
las que, por su forma, tienen un solo tipo posible).

--}

import Crosscutting

type VarId = Int

data LetT = LetT TypeF Ssa

data Ssa
  = AppT TypeF FunF [Ssa]
  | BoundT VarId
  | FreeT TypeF Name
  | ConstT Float
  | PrjT FieldF Ssa
  deriving Show
