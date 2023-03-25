{-

Este modulo implementa la ultima etapa de compilacion.
Especificamente, compila la representacion intermedia
`FormNl` al lenguage objetivo, GLSL.

-}
module Codegen
  ( emitGlsl
  ) where


import Core
import Crosscutting
import GlPrinter

-- Dada una lista de instrucciones, genera un programa en
-- GLSL que les corresponde.
emitGlsl :: FormNl -> [DeclN] -> String
emitGlsl c cs = showStmt $ glSeq block returnStmt
  where
  block = foldl1 glSeq statements
  statements = map (uncurry go) $ zip [0..] cs
  returnStmt = glReturn $ renderValue c

go :: VarId -> DeclN -> GlStmt
go idx (DeclN t v@(LitN _)) = renderConstDecl (renderType t) idx (renderValue v)
go idx (DeclN t v)          = renderDecl      (renderType t) idx (renderValue v)

renderValue :: FormNl -> GlExpr
renderValue (LitN x)       = glFloatLiteral x
renderValue (FreeN x)      = glNameExpr $ glName x
renderValue (BoundN i)     = glNameExpr $ renderVar i
renderValue (AppN f as)    = renderApp f as
renderValue (PrjN field a) = glFieldAccess (renderField field) (renderValue a)

renderApp :: FunF -> [FormNl] -> GlExpr
renderApp SubF [a0, a1] = glBinop "-" (renderValue a0) (renderValue a1)
renderApp AddF [a0, a1] = glBinop "+" (renderValue a0) (renderValue a1)
renderApp MulF [a0, a1] = glBinop "*" (renderValue a0) (renderValue a1)
renderApp DivF [a0, a1] = glBinop "/" (renderValue a0) (renderValue a1)
renderApp f as = glCallExpr (renderFun f) (map renderValue as)

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

renderVar :: VarId -> GlName
renderVar n = glName ("v" ++ show n)

renderField :: FieldF -> GlName
renderField XF = glName "x"
renderField YF = glName "y"
renderField ZF = glName "z"
