module Examples where

import Shape

testShape :: Shape
testShape = union [smoothUnion 0.05 b c, s]
  where s = capsule 0.1 0.7
        c = sphere 0.4
        b = translate (0.5, -0.3, 0.0) $ roundbox 0.05 (0.3, 0.7, 1.1)

simpleTest :: Shape
simpleTest = smoothUnion 0.05 c s
  where s = capsule 0.1 0.7
        c = sphere 0.4

pointer :: Shape
pointer = union [ roundbox 0.025 (0.1, 0.4, 1.0)
                , translate (0, 0.5, 0) $ sphere 0.1 ]

rotatedBox :: Shape
rotatedBox = rotateZ 0.785398 pointer

