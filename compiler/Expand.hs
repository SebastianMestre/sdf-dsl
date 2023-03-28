{-

Este modulo convierte un Shape a la formula matematica
correspondiente a su SDF.

-}
module Expand where

import ShapeAst
import Form
import Crosscutting

expand :: Shape -> Form
expand PointS                        = lengthF pos
expand (TranslatedS x s)             = withPos (pos - vec x) (expand s)
expand (InflatedS x s)               = expand s - constantF x
expand (ExtrudedS x s)               =
  let pos' = pos - clampF pos (vec (negate3 x)) (vec x)
      inside = minF 0.0 (maxComp (abs pos - vec x))
  in withPos pos' (expand s) + inside
expand (TransformedS a s)            = withPos (mat (transpose3 a) * pos) (expand s)
expand (ScaledS x s)                 = constantF x * withPos (vec (ix, ix, ix) * pos) (expand s)
  where ix = 1 / x
expand (RepeatedXS x s)              = withPos (periodic x pos) (expand s)
expand (UnionS s1 s2)                = minF (expand s1) (expand s2)
expand (IntersectionS s1 s2)         = maxF (expand s1) (expand s2)
expand (SmoothUnionS k s1 s2)        =
  -- compilamos a b y h con lets para que se calculen una sola vez
  withLocal "a" ScalarF (expand s1) $ \a ->
  withLocal "b" ScalarF (expand s2) $ \b ->
  withLocal "h" ScalarF (maxF (constantF k - abs (a - b) ) 0 / constantF k) $ \h ->
  minF b a - h * h * h * constantF (k / 6)
expand (SmoothIntersectionS k s1 s2) =
  withLocal "a" ScalarF (expand s1) $ \a ->
  withLocal "b" ScalarF (expand s2) $ \b ->
  withLocal "h" ScalarF (maxF (constantF k - abs (a - b) ) 0 / constantF k) $ \h ->
  maxF a b + h * h * h * constantF (k / 6)


periodic :: Float -> Form -> Form
periodic x f =
  withLocal "old" VectorF f $ \old ->
  let period = constantF x
      offset = constantF (x / 2)
      newX = modF ((getX old) + offset) period - offset
      newY = getY old
      newZ = getZ old
  in mkVecF newX newY newZ

withPos :: Form -> Form -> Form
withPos pos' t = withLocal "pos" VectorF pos' (const t)

vec :: Float3 -> Form
vec (x, y, z)    = mkVecF (constantF x) (constantF y) (constantF z)

mat :: Float3x3 -> Form
mat (vx, vy, vz) = mkMatF (vec vx) (vec vy) (vec vz)

negate3 :: Float3 -> Float3
negate3 (x, y, z) = (-x, -y, -z)

transpose3 :: Float3x3 -> Float3x3
transpose3 ((a11, a12, a13), (a21, a22, a23), (a31, a32, a33)) = ((a11, a21, a31), (a12, a22, a32), (a13, a23, a33))

-- genera una formula que evalua a la componente mas grande de la entrada
maxComp :: Form -> Form
maxComp f =
  withLocal "v" VectorF f $ \v ->
  maxF (maxF (getX v) (getY v)) (getZ v)
