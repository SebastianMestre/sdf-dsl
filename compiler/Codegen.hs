module Codegen
  ( emitGlsl
  ) where

-- Este modulo implementa la ultima etapa de compilacion.
-- Especificamente, compila la representacion intermedia
-- `Ssa` al lenguage objetivo, GLSL.

import Crosscutting
import GlPrinter
import Ssa

-- Dada una lista de instrucciones, genera un programa en
-- GLSL que les corresponde.
emitGlsl :: [Ssa] -> String
emitGlsl cs = showStmt block
  where
  block = foldl1 glSeq statements
  statements = map (uncurry go) $ zip [0..] cs

go :: VarId -> Ssa -> GlStmt
go idx v@(ConstT x)         = renderConstDecl glFloat idx (renderValue v)
go idx v@(VarT t x)         = renderDecl (renderType t) idx (renderValue v)
go idx v@(AppT t f as)      = renderDecl (renderType t) idx (renderValue v)
go idx v@(RichPrjT field a) = renderDecl glFloat idx (renderValue v)

renderValue :: Ssa -> GlExpr
renderValue (ConstT x)         = glFloatLiteral x
renderValue (VarT t x)         = glNameExpr $ glName x
renderValue (AppT t f as)      = renderApp f as
renderValue (RichPrjT field a) = glFieldAccess (renderField field) (renderValue a)

renderApp :: FunF -> [SsaArg] -> GlExpr
renderApp SubF [a0, a1] = glBinop "-" (renderAtom a0) (renderAtom a1)
renderApp AddF [a0, a1] = glBinop "+" (renderAtom a0) (renderAtom a1)
renderApp MulF [a0, a1] = glBinop "*" (renderAtom a0) (renderAtom a1)
renderApp DivF [a0, a1] = glBinop "/" (renderAtom a0) (renderAtom a1)
renderApp f as = glCallExpr (renderFun f) (map renderAtom as)

renderFun :: FunF -> GlName
renderFun LengthF = glName "length"
renderFun MkVecF  = glName "vec3"
renderFun MkMatF  = glName "mat3"
renderFun ClampF  = glName "clamp"
renderFun MinF    = glName "min"
renderFun MaxF    = glName "max"
renderFun ModF    = glName "mod"
renderFun MixF    = glName "mix"
renderFun AbsF    = glName "abs"

renderDecl :: GlType -> VarId -> GlExpr -> GlStmt
renderDecl ty idx expr = glDecl ty (renderVar idx) expr

renderConstDecl :: GlType -> VarId -> GlExpr -> GlStmt
renderConstDecl ty idx expr = glConstDecl ty (renderVar idx) expr

renderType :: TypeF -> GlType
renderType ScalarF = glFloat
renderType VectorF = glVec3
renderType MatrixF = glMat3

renderAtom :: SsaArg -> GlExpr
renderAtom (SsaVar n)   = glNameExpr (renderVar n)
renderAtom (SsaConst x) = glFloatLiteral x

renderVar :: VarId -> GlName
renderVar n = glName ("v" ++ show n)

renderField :: FieldF -> GlName
renderField XF = glName "x"
renderField YF = glName "y"
renderField ZF = glName "z"
