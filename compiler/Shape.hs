module Shape (
    Shape,
    Float3,
    Float3x3,

    point,
    sphere,
    box,
    roundbox,
    capsule,

    rotateZ,
    translate,
    inflate,
    union,
    smoothUnion,

    compile
  ) where

import Ast
import qualified VectorCompiler
import Crosscutting

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
rotateZ = RotatedXyS


compile :: Shape -> String
compile s = targetProgram
  where
    untypedFormula = VectorCompiler.expand s
    typedFormula   = VectorCompiler.infer [("pos", VectorF)] untypedFormula
    -- es medio turbio el unwrap, pero solo causa problema si hay bugs en expand o infer.
    ssaProgram     = VectorCompiler.lower (snd $ unwrap $ typedFormula)
    targetProgram  = VectorCompiler.emitGlsl ssaProgram

    unwrap (Right x) = x
