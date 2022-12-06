module Expand where

import ShapeAst
import Formula
import Crosscutting

expand :: Shape -> Formula
expand PointS                 = lengthF pos
expand (TranslatedS x s)      = withPos (pos - vec x) (expand s)
expand (InflatedS x s)        = expand s - constantF x
expand (ExtrudedS x s)        = withPos (extrude x pos) (expand s)
expand (TransformedS a s)     = withPos (mat (transpose3 a) * pos) (expand s)
expand (RepeatedXS x s)       = withPos (periodic x pos) (expand s)
expand (UnionS s1 s2)         = minF (expand s1) (expand s2)
expand (IntersectionS s1 s2)  = maxF (expand s1) (expand s2)
expand (SmoothUnionS k s1 s2) =
  withLocal "a" ScalarF (expand s1) $ \a ->
  withLocal "b" ScalarF (expand s2) $ \b ->
  withLocal "h" ScalarF (clampF (0.5 + 0.5 * (b - a) / constantF k) 0 1) $ \h ->
  mixF b a h - constantF k * h * (1 - h)

extrude :: Float3 -> Formula -> Formula
extrude x f = f - clampF f (vec $ negate3 x) (vec x)

periodic :: Float -> Formula -> Formula
periodic x f =
  withLocal "old" VectorF f $ \old ->
  let period = constantF x
      offset = constantF (x / 2)
      newX = modF ((getX old) + offset) period - offset
      newY = getY old
      newZ = getZ old
  in mkVecF newX newY newZ

withPos :: Formula -> Formula -> Formula
withPos pos' t = withLocal "pos" VectorF pos' (const t)


vec (x, y, z)    = mkVecF (constantF x) (constantF y) (constantF z)
mat (vx, vy, vz) = mkMatF (vec vx) (vec vy) (vec vz)

negate3 :: Float3 -> Float3
negate3 (x, y, z) = (-x, -y, -z)

transpose3 :: Float3x3 -> Float3x3
transpose3 ((a11, a12, a13), (a21, a22, a23), (a31, a32, a33)) = ((a11, a21, a31), (a12, a22, a32), (a13, a23, a33))
