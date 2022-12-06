module Expand where

import ShapeAst
import Formula
import Crosscutting

pos = varF "pos"

expand :: Shape -> Formula
expand PointS                 = lengthF pos
expand (TranslatedS x s)      = withPos (pos - vec x) (expand s)
expand (InflatedS x s)        = expand s - constantF x
expand (ExtrudedS x s)        = withPos (extrude x pos) (expand s)
expand (RotatedXyS a s)       = withPos (mat (makeRotationMatrix a) * pos) (expand s)
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

withLocal :: String -> TypeF -> Formula -> (Formula -> Formula) -> Formula
withLocal v ty t f = letF ty v t (f (varF v))

vec (x, y, z)    = mkVecF (constantF x) (constantF y) (constantF z)
mat (vx, vy, vz) = mkMatF (vec vx) (vec vy) (vec vz)

makeRotationMatrix :: Float -> Float3x3
makeRotationMatrix angle =
  ((cosAngle, -sinAngle, 0),
   (sinAngle,  cosAngle, 0),
   (0,         0,        1))
  where
  cosAngle = cos (-angle)
  sinAngle = sin (-angle)

negate3 (x, y, z) = (-x, -y, -z)
