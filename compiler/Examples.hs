module Examples where

import Shape

-- una caja con una esfera en la punta
pointer :: Shape
pointer = union [ roundbox 0.025 (0.1, 0.4, 1.0)
                , translate (0, 0.5, 0) $ sphere 0.1 ]

-- el objeto de arriba, rotado en el eje Z
rotatedBox :: Shape
rotatedBox = rotateZ 0.8 pointer

-- una patita de gato
patita :: Shape
patita = union [brazo, almohadillas, palma]
  where

  dedo0 = translate (-0.070, 0.26, -0.015) $ sphere 0.06
  dedo1 = translate (-0.020, 0.29, -0.010) $ sphere 0.07
  dedo2 = translate ( 0.045, 0.28, -0.010) $ sphere 0.07
  dedo3 = translate ( 0.100, 0.24, -0.020) $ sphere 0.05
  dedos = union [dedo0, dedo1, dedo2, dedo3]

  almohadilla0 = translate (-0.070, 0.26, -0.010) $ sphere 0.03
  almohadilla1 = translate (-0.020, 0.29, -0.010) $ sphere 0.03
  almohadilla2 = translate ( 0.045, 0.28, -0.010) $ sphere 0.03
  almohadilla3 = translate ( 0.100, 0.24, -0.006) $ sphere 0.03
  almohadillas = translate (0.0, 0.0, -0.05)
               $ union [ almohadilla0
                       , almohadilla1
                       , almohadilla2
                       , almohadilla3 ]

  palma0 = sphere 0.07
  palma1 = translate (-0.04, -0.04, -0.025) $ sphere 0.04
  palma2 = translate ( 0.04, -0.04, -0.025) $ sphere 0.04
  palma  = translate (0.0, 0.2, -0.01)
         $ smoothUnion 0.02 palma0 (union [palma1, palma2])

  forma = roundbox 0.05 (0.1, 0.3, 0.06)
  brazo = smoothUnion 0.03 forma dedos
