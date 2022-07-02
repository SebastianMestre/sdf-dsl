module Shape where

type Float3 = (Float, Float, Float)

-- DSL for expressing shapes
data Shape
  = PointS
  | TranslatedS Float3 Shape
  | ExtrudedS Float3 Shape
  | InflatedS Float Shape
  | UnionS Shape Shape
  | RotatedXyS Float Shape -- rotates in the xy plane
  | RepeatedXS Float Shape -- repeats linearly in the x axis
  | RepeatedXyS Int Shape -- repeats radially in the xy plane
  deriving Show

