module Expand where

import Shape
import Formula
import Crosscutting

vpos = VarF () "pos"

vecLiteral (x, y, z) = AppF () MkVecF [LitF x, LitF y, LitF z]

matLiteral (vx, vy, vz) = AppF () MkMatF [vecLiteral vx, vecLiteral vy, vecLiteral vz]

makeRotationMatrix :: Float -> (Float3, Float3, Float3)
makeRotationMatrix angle = 
  ((cosAngle, -sinAngle, 0),
   (sinAngle,  cosAngle, 0),
   (0,         0,        1))
  where
  cosAngle = cos (-angle)
  sinAngle = sin (-angle)

negate3 (x, y, z) = (-x, -y, -z)

extrude radii x = v - AppF () ClampF [v, vecLiteral (negate3 radii), vecLiteral radii]
  where v = VarF () x 

expand :: Shape -> Form ()
expand PointS =
  AppF () LengthF [vpos]
expand (TranslatedS delta s) =
  LetF () VectorF "pos" (vpos - vecLiteral delta) $
  expand s
expand (InflatedS k s) =
  expand s - LitF k
expand (ExtrudedS radii s) =
  LetF () VectorF "pos" (extrude radii "pos") $
  expand s
expand (RotatedXyS angle s) =
  LetF () VectorF "pos" (matLiteral (makeRotationMatrix angle) * vpos) $
  expand s
expand (RepeatedXS interval s) = expand s -- TODO
expand (RepeatedXyS times s) = expand s -- TODO
expand (UnionS s1 s2) =
  AppF () MinF [expand s1, expand s2]

