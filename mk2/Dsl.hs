module Dsl where

import Shape

union = foldr1 UnionS

sphere :: Float -> Shape
sphere radius  = InflatedS radius PointS

box :: Float3 -> Shape
box dimensions = ExtrudedS dimensions PointS

roundbox :: Float -> Float3 -> Shape
roundbox radius (dx, dy, dz) = InflatedS radius $ box (dx - radius, dy - radius, dz - radius)

capsule :: Float -> Float -> Shape
capsule radius length = ExtrudedS (length, 0.0, 0.0) (sphere radius)

crown :: Int -> Float -> Float -> Shape
{--
crown n radius = union spheres
  where spheres = [TranslatedS centre (sphere radius) | centre <- centres]
        centres = [(cos angle, sin angle, 0.0) | angle <- angles]
        angles  = [(6.283185 / fromIntegral n) * fromIntegral i | i <- [1..n]]
--}
crown n bigRadius radius = RepeatedXyS n (TranslatedS (bigRadius, 0, 0) (sphere radius))

testShape :: Shape
testShape = union [b, c, s]
  where s = capsule 0.1 0.7
        c = sphere 0.4
        b = TranslatedS (0.5, -0.3, 0.0) $ roundbox 0.05 (0.3, 0.7, 1.1)

pointer :: Shape
pointer = UnionS (roundbox 0.025 (0.1, 0.4, 1.0)) (TranslatedS (0, 0.5, 0) $ sphere 0.1)

rotatedBox :: Shape
rotatedBox = RotatedXyS 0.785398 pointer

smallCrown = crown 12 0.16 0.04

dotRow = RepeatedXS 0.4 smallCrown

dotMatrix = RepeatedXS 0.4 $ RotatedXyS 1.57079632 dotRow
