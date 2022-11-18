module VectorCompiler where

import Typechecking
import Lower
import Expand
import Codegen

expand = Expand.expand
infer = Typechecking.infer
lower = Lower.lower
emitJs = Codegen.emitJs
emitGlsl = Codegen.emitGlsl
