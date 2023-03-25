module Shape (
    Shape,
    Float3,
    Float3x3,

    -- primitives
    point,
    sphere,
    box,
    roundbox,
    capsule,

    -- transformations
    rotateZ,
    translate,
    inflate,
    repeatX,

    -- combinators
    union,
    smoothUnion,

    -- evaluation
    compile
  ) where

import ShapeAst
import Crosscutting

import Expand (expand)
import Typechecking (infer)
import HoistLets (hoistLets)
import Codegen (emitGlsl)

point :: Shape
point = PointS

sphere :: Float -> Shape
sphere radius  = InflatedS radius PointS

box :: Float3 -> Shape
box dimensions = ExtrudedS dimensions PointS

roundbox :: Float -> Float3 -> Shape
roundbox radius (dx, dy, dz) = InflatedS radius $ box (dx - radius, dy - radius, dz - radius)

capsule :: Float -> Float -> Shape
capsule radius length = ExtrudedS (length, 0.0, 0.0) (sphere radius)


union :: [Shape] -> Shape
union = foldr1 UnionS

smoothUnion :: Float -> Shape -> Shape -> Shape
smoothUnion = SmoothUnionS

inflate :: Float -> Shape -> Shape
inflate = InflatedS

translate :: Float3 -> Shape -> Shape
translate = TranslatedS

rotateZ :: Float -> Shape -> Shape
rotateZ angle = TransformedS rotationMatrix
  where
  rotationMatrix =
    ((cosAngle, -sinAngle, 0),
     (sinAngle,  cosAngle, 0),
     (0,         0,        1))
  cosAngle = cos angle
  sinAngle = sin angle

repeatX :: Float -> Shape -> Shape
repeatX = RepeatedXS

compile :: Shape -> String
compile s = targetProgram
  where
    formula = expand s
    -- es medio turbio el unwrap, pero solo puede
    -- dar error si hay bugs en expand o infer.
    formulaType                  = unwrap $ infer [("pos", VectorF)] formula
    (lastExpression, ssaProgram) = hoistLets formula
    targetProgram                = emitGlsl lastExpression ssaProgram

    unwrap (Right x) = x
