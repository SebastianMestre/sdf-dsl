module Ssa where

{--

Este módulo define una version de Form sin let anidado
que se usa como representación intermedia en la
compilación a GLSL.

Los programas con variables intermedias se representan con
`[DeclT]` y un resultado final de tipo `Ssa`.

--}

import Crosscutting

type VarId = Int

data DeclT = DeclT TypeF Ssa

data Ssa
  = AppT FunF [Ssa]
  | BoundT VarId
  | FreeT Name
  | ConstT Float
  | PrjT FieldF Ssa
  deriving Show
