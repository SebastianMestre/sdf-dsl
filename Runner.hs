module Runner where

import Shape
import Dsl
import VectorCompiler
import Crosscutting

-- bx = TranslatedS (0.0, 0.0, 1.0) dotMatrix
-- bx = TranslatedS (0.0, 0.0, 1.0) smallCrown
-- bx = TranslatedS (-0.5, 0, 1) $ UnionS (sphere 1) (TranslatedS (1,0,0) $ box (1,1,1))
bx = TranslatedS (0, 0, 1) $ sphere 0.2
bx' = VectorCompiler.expand bx
bx'' = VectorCompiler.infer [("pos", VectorF)] bx'
bx''' = VectorCompiler.lower (snd $ unMaybe $ bx'')
jsbx = VectorCompiler.emitJs bx'''
glbx = VectorCompiler.emitGlsl bx'''

go x = glx
  where
  x' = VectorCompiler.expand x
  x'' = VectorCompiler.infer [("pos", VectorF)] x'
  x''' = VectorCompiler.lower (snd $ unMaybe $ x'')
  glx = VectorCompiler.emitGlsl x'''

unMaybe (Right x) = x

