import Shape
import Dsl
import VectorCompiler
import Crosscutting

dedo0 = TranslatedS (-0.070, 0.26, -0.015) $ sphere 0.06
dedo1 = TranslatedS (-0.020, 0.29, -0.010) $ sphere 0.07
dedo2 = TranslatedS ( 0.045, 0.28, -0.010) $ sphere 0.07
dedo3 = TranslatedS ( 0.100, 0.24, -0.020) $ sphere 0.05
dedos = union [dedo0, dedo1, dedo2, dedo3]

almohadilla0 = TranslatedS (-0.070, 0.26, -0.010) $ sphere 0.03
almohadilla1 = TranslatedS (-0.020, 0.29, -0.010) $ sphere 0.03
almohadilla2 = TranslatedS ( 0.045, 0.28, -0.010) $ sphere 0.03
almohadilla3 = TranslatedS ( 0.100, 0.24, -0.006) $ sphere 0.03
almohadillas  = TranslatedS (0.0, 0.0, -0.05) $ union [almohadilla0, almohadilla1, almohadilla2, almohadilla3]

palma0 = sphere 0.07
palma1 = TranslatedS (-0.04, -0.04, -0.025) $ sphere 0.04
palma2 = TranslatedS ( 0.04, -0.04, -0.025) $ sphere 0.04
palma  = TranslatedS (0.0, 0.2, -0.01) $ SmoothUnionS 0.02 palma0 (UnionS palma1 palma2)

forma = roundbox 0.05 (0.1, 0.3, 0.06)
brazo = SmoothUnionS 0.03 forma dedos

patita = union [brazo, almohadillas, palma]

unMaybe (Right x) = x

programa :: String
programa = paso4
  where paso1 = VectorCompiler.expand patita
        paso2 = VectorCompiler.infer [("pos", VectorF)] paso1
        paso3 = VectorCompiler.lower (snd $ unMaybe paso2)
        paso4 = VectorCompiler.emitGlsl paso3

main :: IO ()
main = putStrLn programa
