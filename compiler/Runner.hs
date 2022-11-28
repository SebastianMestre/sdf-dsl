module Runner where

import Shape
import VectorCompiler
import Crosscutting

bx = translate (0, 0, 1) $ sphere 0.2
glbx = compile bx

go x = compile x
  where
  x' = VectorCompiler.expand x
  x'' = VectorCompiler.infer [("pos", VectorF)] x'
  x''' = VectorCompiler.lower (snd $ unMaybe $ x'')
  glx = VectorCompiler.emitGlsl x'''


