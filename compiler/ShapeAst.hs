module ShapeAst where

type Float3 = (Float, Float, Float)

type Float3x3 = (Float3, Float3, Float3)

-- DSL para expresar cuerpos geometricos
data Shape
  = PointS

-- Posicion
  | TranslatedS Float3 Shape
  | TransformedS Float3x3 Shape

-- Manipulation
  | ExtrudedS Float3 Shape
  | InflatedS Float Shape
  | ScaledS   Float Shape

-- Operaciones de conjunto
  | UnionS Shape Shape
  | IntersectionS Shape Shape

  | SmoothUnionS Float Shape Shape

-- Simetrias
  | RepeatedXS Float Shape

  deriving Show
