{-

Este modulo define los AST de los lenguajes intermedios.

-}
module Core where

import Crosscutting



{-

Form corresponde con un lenguaje para representar
expresiones matemáticas. Cada let está anotado con el tipo
de la variable que liga.

-}

data Form
  = LetF TypeF Name Form Form
  | VarF Name
  | AppF FunF [Form]
  | LitF Float
  | PrjF FieldF Form
  deriving Show



{-

FormNl es una variante de Form sin let que se usa como
representación intermedia en la compilación a GLSL, que no
tiene expresiones let.

Los programas Form que usan variables intermedias se
representan con una lista de declaraciones `[DeclN]` y un
resultado final de tipo `FormNl`.

Las variables ligadas se identifican por su indice en la
lista de declaraciones.

-}

type VarId = Int
data DeclN = DeclN TypeF FormNl
data FormNl
  = AppN FunF [FormNl]
  | BoundN VarId
  | FreeN Name
  | LitN Float
  | PrjN FieldF FormNl
  deriving Show
