module Runner where

import Dsl
import VectorCompiler
import Crosscutting

bx = rotatedBox
bx' = VectorCompiler.expand bx
bx'' = VectorCompiler.infer [("pos", VectorF)] bx'
bx''' = VectorCompiler.lower (snd $ unMaybe $ bx'')
jsbx = VectorCompiler.emitJs bx'''
glbx = VectorCompiler.emitGlsl bx'''

unMaybe (Right x) = x
