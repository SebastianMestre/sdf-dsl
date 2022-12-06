module ShapeAst where

type Float3 = (Float, Float, Float)

type Float3x3 = (Float3, Float3, Float3)

-- DSL for expressing shapes
data Shape
  = PointS                         -- Point at the origin

-- Positioning
  | TranslatedS Float3 Shape       -- Moves a shape in space
  | TransformedS Float3x3 Shape       -- Applies a linear transformation (assumes orthonormal matrix)

-- Shape manipulation
  | ExtrudedS Float3 Shape         -- Separates two halves of a shape, and fills in the middle
  | InflatedS Float Shape          -- Inflates a shape, rounding its corners

-- Set operations
  | UnionS Shape Shape             -- Set union of two shapes
  | IntersectionS Shape Shape      -- Set intersection of two shapes
--  | DifferenceS Shape Shape        -- Set difference of two shapes

  | SmoothUnionS Float Shape Shape -- Blob union of two shapes

-- Symmetries
  | RepeatedXS Float Shape         -- repeats linearly in the x axis
--  | RepeatedXyS Int Shape          -- repeats radially in the xy plane

  deriving Show
