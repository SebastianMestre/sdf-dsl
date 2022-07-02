module Expand where

import Shape
import Formula
import Crosscutting

varF = VarF ()
appF = AppF ()
letF = LetF ()
prjF = PrjF ()

vpos = varF "pos"

getX = prjF XF
getY = prjF YF
getZ = prjF ZF

withX x e =
  letF VectorF "__old" e $
  appF MkVecF [x, getY old, getZ old]
  where old = varF "__old"

modF x y = appF ModF [x, y]

vecLiteral (x, y, z) = appF MkVecF [LitF x, LitF y, LitF z]

matLiteral (vx, vy, vz) = appF MkMatF [vecLiteral vx, vecLiteral vy, vecLiteral vz]

makeRotationMatrix :: Float -> (Float3, Float3, Float3)
makeRotationMatrix angle =
  ((cosAngle, -sinAngle, 0),
   (sinAngle,  cosAngle, 0),
   (0,         0,        1))
  where
  cosAngle = cos (-angle)
  sinAngle = sin (-angle)

negate3 (x, y, z) = (-x, -y, -z)

extrude radii x = v - appF ClampF [v, vecLiteral (negate3 radii), vecLiteral radii]
  where v = varF x

expand :: Shape -> Form ()
expand PointS =
  appF LengthF [vpos]
expand (TranslatedS delta s) =
  letF VectorF "pos" (vpos - vecLiteral delta) $
  expand s
expand (InflatedS k s) =
  expand s - LitF k
expand (ExtrudedS radii s) =
  letF VectorF "pos" (extrude radii "pos") $
  expand s
expand (RotatedXyS angle s) =
  letF VectorF "pos" (matLiteral (makeRotationMatrix angle) * vpos) $
  expand s
expand (RepeatedXS interval s) =
  letF ScalarF "offset" (LitF (interval / 2)) $
  letF VectorF "pos" (withX (modF (getX vpos + offset) (LitF interval) - offset) vpos) $
  expand s
  where offset = varF "offset"
expand (RepeatedXyS times s) = expand s -- TODO
expand (UnionS s1 s2) =
  appF MinF [expand s1, expand s2]

