module Shape (
    Shape,
    Float3,
    Float3x3,

    -- primitivas
    point,              -- Punto en el origen
    sphere,             -- Esfera
    box,                -- Prisma rectangular
    roundbox,           -- Prisma rectangular con esquinas redondeadas
    capsule,            -- Capsula

    -- transformaciones
    scale,              -- Escala una Shape
    rotateX,            -- Rota una Shape sobre el eje X
    rotateY,            -- Rota una Shape sobre el eje Y
    rotateZ,            -- Rota una Shape sobre el eje Z
    translate,          -- Mueve un Shape en el espacio
    inflate,            -- Infla un Shape, redondeando sus esquinas
    repeatX,            -- Repite un Shape sobre el eje X indefinidamente

    -- combinadores
    union,              -- Union de varios Shapes
    intersection,       -- Interseccion de varios Shapes
    smoothUnion,        -- Union de dos Shapes, suavizando el punto de contacto
    smoothIntersection, -- Interseccion de dos Shapes, suavizando el punto de contacto

    -- evaluacion
    compile             -- Da una codificacion de un Shape en GLSL
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

intersection :: [Shape] -> Shape
intersection = foldr1 IntersectionS

smoothUnion :: Float -> Shape -> Shape -> Shape
smoothUnion = SmoothUnionS

smoothIntersection :: Float -> Shape -> Shape -> Shape
smoothIntersection = SmoothIntersectionS

inflate :: Float -> Shape -> Shape
inflate = InflatedS

translate :: Float3 -> Shape -> Shape
translate = TranslatedS

rotateX :: Float -> Shape -> Shape
rotateX angle = TransformedS rotationMatrix
  where
  rotationMatrix =
    ((1, 0,         0       )
    ,(0, cosAngle, -sinAngle)
    ,(0, sinAngle,  cosAngle))
  cosAngle = cos angle
  sinAngle = sin angle

rotateY :: Float -> Shape -> Shape
rotateY angle = TransformedS rotationMatrix
  where
  rotationMatrix =
    (( cosAngle, 0, sinAngle)
    ,( 0,        1, 0       )
    ,(-sinAngle, 0, cosAngle))
  cosAngle = cos angle
  sinAngle = sin angle

rotateZ :: Float -> Shape -> Shape
rotateZ angle = TransformedS rotationMatrix
  where
  rotationMatrix =
    ((cosAngle, -sinAngle, 0)
    ,(sinAngle,  cosAngle, 0)
    ,(0,         0,        1))
  cosAngle = cos angle
  sinAngle = sin angle

scale :: Float -> Shape -> Shape
scale = ScaledS

repeatX :: Float -> Shape -> Shape
repeatX = RepeatedXS

compile :: Shape -> String
compile s = targetProgram
  where
    formula = expand s

    -- unwrap diverge si infer falla, pero esto solo pasa si
    -- tenemos un bug en expand.
    formulaType       = unwrap $ infer [("pos", VectorF)] formula
    (lastExpr, decls) = hoistLets formula
    targetProgram     = emitGlsl lastExpr decls

    unwrap (Right x) = x
